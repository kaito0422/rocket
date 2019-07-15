// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import Chisel._
import Chisel.ImplicitConversions._
import chisel3.internal.InstanceId
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.tile.HasCoreParameters
import freechips.rocketchip.util._

/*
 * BHT的参数（Branch History Table）
 * Entry数为512个，可以记录512个最近遇到的PC
 * Counter的长度为1，Predictor只能给出预测，不能给出预测强度（即没有SN，WN，WT，ST）
 * History Bit为3位，对于每一个Entry科技记录最近的8次跳转还是不跳转的历史
 * 因此一个BHT Entry为如下：
 *     |_|_|_|_|_|_|_|_|
 *      | | | | | | | |---> 1BC(Hist=7)
 *      | | | | | | |-----> 1BC(Hist=6)
 *      | | | | |---------> 1BC(Hist=4)
 *      | | | |-----------> 1BC(Hist=3)
 *      | | |-------------> 1BC(Hist=2)
 *      | |---------------> 1BC(Hist=1)
 *      |-----------------> 1BC(Hist=0) 
 *    --------------------> Hist(History 3-Bit)
 * 所以一个BHT Entry为11位
 */
case class BHTParams(
  nEntries: Int = 512,
  counterLength: Int = 1,
  historyLength: Int = 8,
  historyBits: Int = 3)

/*
 * BTB的参数（Branch Target Buffer）
 * 只有28个Entry，可以记录最近产生跳转的28个PC
 * 用于和PC进行比对的位有14位（但是此处看不出使用的PC的哪14位）
 * Page？
 * RAS有6个位置（Return Address Stack）
 */
case class BTBParams(
  nEntries: Int = 28,
  nMatchBits: Int = 14,
  nPages: Int = 6,
  nRAS: Int = 6,
  bhtParams: Option[BHTParams] = Some(BHTParams()),
  updatesOutOfOrder: Boolean = false)

trait HasBtbParameters extends HasCoreParameters { this: InstanceId =>
  val btbParams = tileParams.btb.getOrElse(BTBParams(nEntries = 0))
  val matchBits = btbParams.nMatchBits max log2Ceil(p(CacheBlockBytes) * tileParams.icache.get.nSets)
  val entries = btbParams.nEntries
  val updatesOutOfOrder = btbParams.updatesOutOfOrder
  val nPages = (btbParams.nPages + 1) / 2 * 2 // control logic assumes 2 divides pages
}

abstract class BtbModule(implicit val p: Parameters) extends Module with HasBtbParameters {
  Annotated.params(this, btbParams)
}

abstract class BtbBundle(implicit val p: Parameters) extends Bundle with HasBtbParameters

/*
 * RAS：Return Address Stack
 * 是用来存放返回地址的。当有一条Call指令到来时，会按照程序原本的顺序，把Call指令下面的那一条指令的PC存放到RAS中
 * 当到来的是一条Return指令时，就会从RAS中弹出最上方的一条指令，作为Target PC
 * （Jump指令，return指令 还是 branch指令的工作是由BTB中的table完成的）
 */
class RAS(nras: Int) {              // 该结构本质上其实是一个Stack，其中的push，pop和peek都和程序中的Stack概念一样（其中peek相当于是top）
  def push(addr: UInt): Unit = {
    when (count < nras) { count := count + 1 }
    val nextPos = Mux(Bool(isPow2(nras)) || pos < nras-1, pos+1, UInt(0))
    stack(nextPos) := addr
    pos := nextPos
  }
  def peek: UInt = stack(pos)
  def pop(): Unit = when (!isEmpty) {
    count := count - 1
    pos := Mux(Bool(isPow2(nras)) || pos > 0, pos-1, UInt(nras-1))
  }
  def clear(): Unit = count := UInt(0)
  def isEmpty: Bool = count === UInt(0)

  private val count = Reg(UInt(width = log2Up(nras+1)))     // 该RAS中存放了多少个地址
  private val pos = Reg(UInt(width = log2Up(nras)))         // 记录栈顶的位置
  private val stack = Reg(Vec(nras, UInt()))                // Stack对象
}

/*
 * BHT的应答类型
 * BHT本质上只需要回答是T/N
 * 这里存在strongly-taken，说明可能是采用2BC（2BC存在4个状态：SN，WN，WT，ST）
 */
class BHTResp(implicit p: Parameters) extends BtbBundle()(p) {
  val history = UInt(width = btbParams.bhtParams.map(_.historyLength).getOrElse(1))
  val value = UInt(width = btbParams.bhtParams.map(_.counterLength).getOrElse(1))
  def taken = value(0)
  def strongly_taken = value === 1
}

// BHT contains table of 2-bit counters and a global history register.
// The BHT only predicts and updates when there is a BTB hit.
// The global history:
//    - updated speculatively in fetch (if there's a BTB hit).
//    - on a mispredict, the history register is reset (again, only if BTB hit).
// The counter table:
//    - each counter corresponds with the address of the fetch packet ("fetch pc").
//    - updated when a branch resolves (and BTB was a hit for that branch).
//      The updating branch must provide its "fetch pc".
/*
 * BHT：Branch History Table
 * 其中包含一个二位计数器表 和 一个全局历史寄存器
 * 只会在BTB命中的时候更新 ？
 *    此处的hit使之在BHT中找到了之前对该PC的预测记录，如果miss的话当然就没有历史可言，所以必须是hit的
 *    也就是在此处的 "only if BTB hit" 意味着对该PC的两次访问没有相差太远，还能够被保存在BTB中
 
 * BHT中每个Entry如下：
 *     |_|_|_|_|_|_|_|_|
 *      | | | | | | | |---> 1BC(Hist=7)  -|
 *      | | | | | | |-----> 1BC(Hist=6)   |
 *      | | | | |---------> 1BC(Hist=4)   |
 *      | | | |-----------> 1BC(Hist=3)    >  二位计数器
 *      | | |-------------> 1BC(Hist=2)   |
 *      | |---------------> 1BC(Hist=1)   |
 *      |-----------------> 1BC(Hist=0)  -|
 *    --------------------> Hist(History 3-Bit) 全局历史

 * 512个entry可以记录最近的512个PC
 
 */
class BHT(params: BHTParams)(implicit val p: Parameters) extends HasCoreParameters {
  /* 用于索引二位历史表 */
  def index(addr: UInt, history: UInt) = {      // 中间函数
    def hashHistory(hist: UInt) = if (params.historyLength == params.historyBits) hist else {
      val k = math.sqrt(3)/2
      val i = BigDecimal(k * math.pow(2, params.historyLength)).toBigInt
      (i.U * hist)(params.historyLength-1, params.historyLength-params.historyBits)
    }
    def hashAddr(addr: UInt) = {
      val hi = addr >> log2Ceil(fetchBytes)
      hi(log2Ceil(params.nEntries)-1, 0) ^ (hi >> log2Ceil(params.nEntries))(1, 0)
    }
    hashAddr(addr) ^ (hashHistory(history) << (log2Up(params.nEntries) - params.historyBits))
  }
  /* 根据PC，去二位历史表中查找结果，是否保存之前对该PC的预测结果 */
  def get(addr: UInt): BHTResp = {    // 访问BHT历史表，输入为一条指令的PC，去BHT的历史表中查找历史情况，BHT会返回T/N
    val res = Wire(new BHTResp)       // BHTResp作为返回结果是一个结构（或者说是一个packet）
    res.value := table(index(addr, history))    // 根据PC索引到BHT中的一个Entry，再根据history选择使用Entry中的哪一个预测器的结果
    res.history := history        // 历史就是BHT中查询到的历史
    res       // res作为返回值，方法体的最后一个表达式就是方法的返回值
  }

  /* 更新BHT */
  /*
   * 根据PC可以索引到BHT中的一个entry，根据BHTResp和Bool变量，可以改变预测强度（SN，WN，WT，ST）
   */
  def updateTable(addr: UInt, d: BHTResp, taken: Bool): Unit = {
    table(index(addr, d.history)) := (params.counterLength match {    // 在BHT的table中找到该PC对应的表项，然后根据taken情况进行更新
      /*   先在BHT中找到对应的Entry      根据taken的情况修改entry中预测器的预测强度    */
      case 1 => taken
      case 2 => Cat(taken ^ d.value(0), d.value === 1 || d.value(1) && taken)
    })
  }

  def resetHistory(d: BHTResp): Unit = {
    history := d.history
  }

  def updateHistory(addr: UInt, d: BHTResp, taken: Bool): Unit = {
    history := Cat(taken, d.history >> 1)
  }

  def advanceHistory(taken: Bool): Unit = {
    history := Cat(taken, history >> 1)
  }

  private val table = Mem(params.nEntries, UInt(width = params.counterLength))    // 二位计数器表
  val history = Reg(UInt(width = params.historyLength))                           // 全局历史寄存器
}

/* 
 * BTB中对于CFI的类型判断结果的结构（CFI：Compress Flighting Instruction） 
 * 该结构由BTB给出对PC的预测结果
 */
object CFIType {
  def SZ = 2
  def apply() = UInt(width = SZ)
  def branch = 0.U
  def jump = 1.U
  def call = 2.U
  def ret = 3.U
}

// BTB update occurs during branch resolution (and only on a mispredict).
//  - "pc" is what future fetch PCs will tag match against.
//  - "br_pc" is the PC of the branch instruction.
/*
 * BTB更新的结构 
 */
class BTBUpdate(implicit p: Parameters) extends BtbBundle()(p) {
  val prediction = new BTBResp
  val pc = UInt(width = vaddrBits)          // Target PC
  val target = UInt(width = vaddrBits)
  val taken = Bool()
  val isValid = Bool()
  val br_pc = UInt(width = vaddrBits)       // 当前branch指令的PC
  val cfiType = CFIType()
}

// BHT update occurs during branch resolution on all conditional branches.
//  - "pc" is what future fetch PCs will tag match against.
/*
 * BHT更新的结构 
 */
class BHTUpdate(implicit p: Parameters) extends BtbBundle()(p) {
  val prediction = new BHTResp
  val pc = UInt(width = vaddrBits)
  val branch = Bool()
  val taken = Bool()
  val mispredict = Bool()
}

/*
 * RAS更新的结构 
 */
class RASUpdate(implicit p: Parameters) extends BtbBundle()(p) {
  val cfiType = CFIType()
  val returnAddr = UInt(width = vaddrBits)
}

//  - "bridx" is the low-order PC bits of the predicted branch (after
//     shifting off the lowest log(inst_bytes) bits off).
//  - "mask" provides a mask of valid instructions (instructions are
//     masked off by the predicted taken branch from the BTB).
/*
 * BTB应答结构
 */
class BTBResp(implicit p: Parameters) extends BtbBundle()(p) {
  val cfiType = CFIType()                         // BTB返回的指令类型的结果
  val taken = Bool()
  val mask = Bits(width = fetchWidth)
  val bridx = Bits(width = log2Up(fetchWidth))
  val target = UInt(width = vaddrBits)
  val entry = UInt(width = log2Up(entries + 1))
  val bht = new BHTResp
}

class BTBReq(implicit p: Parameters) extends BtbBundle()(p) {
   val addr = UInt(width = vaddrBits)
}

// fully-associative branch target buffer
// Higher-performance processors may cause BTB updates to occur out-of-order,
// which requires an extra CAM port for updates (to ensure no duplicates get
// placed in BTB).
class BTB(implicit p: Parameters) extends BtbModule {
  /* BTB对外的IO接口 */
  val io = new Bundle {
    val req = Valid(new BTBReq).flip                // 外部像BTB发出的访问请求
    val resp = Valid(new BTBResp)                   // BTB对外部给出的应答信号
    val btb_update = Valid(new BTBUpdate).flip      // 更新BTB（为一个packet）
    val bht_update = Valid(new BHTUpdate).flip      // 更新BHT（Branch History Table）
    val bht_advance = Valid(new BTBResp).flip       // ？
    val ras_update = Valid(new RASUpdate).flip      // 更新RAS（Return Address Stack）
    val ras_head = Valid(UInt(width = vaddrBits))   // RAS的头部（类似于Stack的top）
    val flush = Bool().asInput                      // 是否冲刷清空BTB的信号
  }

  val idxs = Reg(Vec(entries, UInt(width=matchBits - log2Up(coreInstBytes))))
  val idxPages = Reg(Vec(entries, UInt(width=log2Up(nPages))))
  val tgts = Reg(Vec(entries, UInt(width=matchBits - log2Up(coreInstBytes))))
  val tgtPages = Reg(Vec(entries, UInt(width=log2Up(nPages))))
  val pages = Reg(Vec(nPages, UInt(width=vaddrBits - matchBits)))
  val pageValid = Reg(init = UInt(0, nPages))

  val isValid = Reg(init = UInt(0, entries))
  val cfiType = Reg(Vec(entries, CFIType()))
  val brIdx = Reg(Vec(entries, UInt(width=log2Up(fetchWidth))))

  private def page(addr: UInt) = addr >> matchBits    // 找到BTB总的Page
  private def pageMatch(addr: UInt) = {               // 判断该Page是否有效
    val p = page(addr)
    pageValid & pages.map(_ === p).asUInt
  }
  private def idxMatch(addr: UInt) = {                // 当索引到一个entry时，检查是否个当前PC匹配
    val idx = addr(matchBits-1, log2Up(coreInstBytes))    // PC对比位的位长位matchBits，所以需要[matchBits - 1 : 指令字节对齐需要的位数（即log2Up(coreInstBytes)）]
    idxs.map(_ === idx).asUInt & isValid
  }

  val r_btb_update = Pipe(io.btb_update)
  val update_target = io.req.bits.addr

  val pageHit = pageMatch(io.req.bits.addr)
  val idxHit = idxMatch(io.req.bits.addr)

  val updatePageHit = pageMatch(r_btb_update.bits.pc)
  val (updateHit, updateHitAddr) =
    if (updatesOutOfOrder) {
      val updateHits = (pageHit << 1)(Mux1H(idxMatch(r_btb_update.bits.pc), idxPages))
      (updateHits.orR, OHToUInt(updateHits))
    } else (r_btb_update.bits.prediction.entry < entries, r_btb_update.bits.prediction.entry)

  val useUpdatePageHit = updatePageHit.orR
  val usePageHit = pageHit.orR
  val doIdxPageRepl = !useUpdatePageHit
  val nextPageRepl = Reg(UInt(width = log2Ceil(nPages)))
  val idxPageRepl = Cat(pageHit(nPages-2,0), pageHit(nPages-1)) | Mux(usePageHit, UInt(0), UIntToOH(nextPageRepl))
  val idxPageUpdateOH = Mux(useUpdatePageHit, updatePageHit, idxPageRepl)
  val idxPageUpdate = OHToUInt(idxPageUpdateOH)
  val idxPageReplEn = Mux(doIdxPageRepl, idxPageRepl, UInt(0))

  val samePage = page(r_btb_update.bits.pc) === page(update_target)
  val doTgtPageRepl = !samePage && !usePageHit
  val tgtPageRepl = Mux(samePage, idxPageUpdateOH, Cat(idxPageUpdateOH(nPages-2,0), idxPageUpdateOH(nPages-1)))
  val tgtPageUpdate = OHToUInt(pageHit | Mux(usePageHit, UInt(0), tgtPageRepl))
  val tgtPageReplEn = Mux(doTgtPageRepl, tgtPageRepl, UInt(0))
  // tgt：Target？

  /* 当BTB需要更新，但是在BTB中没有找到对应PC的entry时，需要替换BTB中的一个entry */
  when (r_btb_update.valid && (doIdxPageRepl || doTgtPageRepl)) {
    val both = doIdxPageRepl && doTgtPageRepl
    val next = nextPageRepl + Mux[UInt](both, 2, 1)
    nextPageRepl := Mux(next >= nPages, next(0), next)
  }

  val repl = new PseudoLRU(entries)     // 对于BTB的替换使用的伪LRU替换策略
  val waddr = Mux(updateHit, updateHitAddr, repl.replace)
  val r_resp = Pipe(io.resp)
  when (r_resp.valid && r_resp.bits.taken || r_btb_update.valid) {
    repl.access(Mux(r_btb_update.valid, waddr, r_resp.bits.entry))
  }

  /* BTB需要更新的时候 */
  when (r_btb_update.valid) {
    val mask = UIntToOH(waddr)
    idxs(waddr) := r_btb_update.bits.pc(matchBits-1, log2Up(coreInstBytes))       // index the entry
    tgts(waddr) := update_target(matchBits-1, log2Up(coreInstBytes))              // 要往BTB中更新的Target PC的对比位
    idxPages(waddr) := idxPageUpdate +& 1 // the +1 corresponds to the <<1 on io.resp.valid
    tgtPages(waddr) := tgtPageUpdate
    cfiType(waddr) := r_btb_update.bits.cfiType
    isValid := Mux(r_btb_update.bits.isValid, isValid | mask, isValid & ~mask)
    if (fetchWidth > 1)
      brIdx(waddr) := r_btb_update.bits.br_pc >> log2Up(coreInstBytes)

    require(nPages % 2 == 0)
    val idxWritesEven = !idxPageUpdate(0)

    def writeBank(i: Int, mod: Int, en: UInt, data: UInt) =
      for (i <- i until nPages by mod)
        when (en(i)) { pages(i) := data }

    writeBank(0, 2, Mux(idxWritesEven, idxPageReplEn, tgtPageReplEn),
      Mux(idxWritesEven, page(r_btb_update.bits.pc), page(update_target)))
    writeBank(1, 2, Mux(idxWritesEven, tgtPageReplEn, idxPageReplEn),
      Mux(idxWritesEven, page(update_target), page(r_btb_update.bits.pc)))
    pageValid := pageValid | tgtPageReplEn | idxPageReplEn
  }

  io.resp.valid := (pageHit << 1)(Mux1H(idxHit, idxPages))
  io.resp.bits.taken := true
  io.resp.bits.target := Cat(pages(Mux1H(idxHit, tgtPages)), Mux1H(idxHit, tgts) << log2Up(coreInstBytes))
  io.resp.bits.entry := OHToUInt(idxHit)
  io.resp.bits.bridx := (if (fetchWidth > 1) Mux1H(idxHit, brIdx) else UInt(0))
  io.resp.bits.mask := Cat((UInt(1) << ~Mux(io.resp.bits.taken, ~io.resp.bits.bridx, UInt(0)))-1, UInt(1))
  io.resp.bits.cfiType := Mux1H(idxHit, cfiType)

  // if multiple entries for same PC land in BTB, zap them
  when (PopCountAtLeast(idxHit, 2)) {
    isValid := isValid & ~idxHit
  }
  when (io.flush) {
    isValid := 0
  }

  if (btbParams.bhtParams.nonEmpty) {
    val bht = new BHT(Annotated.params(this, btbParams.bhtParams.get))
    val isBranch = (idxHit & cfiType.map(_ === CFIType.branch).asUInt).orR
    val res = bht.get(io.req.bits.addr)
    when (io.bht_advance.valid) {       // 所有的BHT entry预先存入taken的预测
      bht.advanceHistory(io.bht_advance.bits.bht.taken)
    }
    when (io.bht_update.valid) {        // 当需要对BHT进行更新时
      when (io.bht_update.bits.branch) {    // 当是branch指令时的更新（把真实结果写入）
        /* 首先修改预测强度 */
        bht.updateTable(io.bht_update.bits.pc, io.bht_update.bits.prediction, io.bht_update.bits.taken)
        when (io.bht_update.bits.mispredict) {
          /* 修改History */
          bht.updateHistory(io.bht_update.bits.pc, io.bht_update.bits.prediction, io.bht_update.bits.taken)
        }
      }.elsewhen (io.bht_update.bits.mispredict) {    // 当不是branch指令时，只对history进行修改 ？
        bht.resetHistory(io.bht_update.bits.prediction)
      }
    }
    when (!res.taken && isBranch) { io.resp.bits.taken := false }   // 当not taken时对BHT的更新
    io.resp.bits.bht := res
  }

  if (btbParams.nRAS > 0) {
    val ras = new RAS(btbParams.nRAS)
    val doPeek = (idxHit & cfiType.map(_ === CFIType.ret).asUInt).orR // BTB检测到是Return指令
    io.ras_head.valid := !ras.isEmpty
    io.ras_head.bits := ras.peek
    when (!ras.isEmpty && doPeek) { // 如果RAS中有数据存在，且BTB检测结果为Return指令
      io.resp.bits.target := ras.peek   // 则从RAS中弹出最上面的PC作为Target PC的预测
    }
    when (io.ras_update.valid) {    // 当RAS需要更新的时候
      when (io.ras_update.bits.cfiType === CFIType.call) {  // 指令处理完已经知道是Call指令
        ras.push(io.ras_update.bits.returnAddr)
      }.elsewhen (io.ras_update.bits.cfiType === CFIType.ret) {   // 如果是Return指令
        ras.pop()
      }
    }
  }
}
