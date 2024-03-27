/* LzmaDec.h -- LZMA Decoder
2017-04-03 : Igor Pavlov : Public domain */
module iv.vfs.lzmadec;
private nothrow @trusted @nogc:

//version = _LZMA_PROB32;
/* _LZMA_PROB32 can increase the speed on some CPUs,
   but memory usage for CLzmaDec::probs will be doubled in that case */

//version = _LZMA_SIZE_OPT;

alias Byte = ubyte;
alias UInt16 = ushort;
alias UInt32 = uint;
alias SizeT = size_t;

version(_LZMA_PROB32) {
  alias CLzmaProb = UInt32;
} else {
  alias CLzmaProb = UInt16;
}

public enum SRes {
  OK,
  ERROR_UNSUPPORTED,
  ERROR_MEM,
  ERROR_DATA,
  ERROR_INPUT_EOF,
  ERROR_FAIL,
}

/* ---------- LZMA Properties ---------- */

public enum LZMA_PROPS_SIZE = 5;

public struct CLzmaProps {
  uint lc, lp, pb;
  UInt32 dicSize;
}

/* LzmaProps_Decode - decodes properties
Returns:
  SRes.OK
  SRes.ERROR_UNSUPPORTED - Unsupported properties
*/

//!SRes LzmaProps_Decode(CLzmaProps *p, const(Byte)* data, uint size);


/* ---------- LZMA Decoder state ---------- */

/* LZMA_REQUIRED_INPUT_MAX = number of required input bytes for worst case.
   Num bits = log2((2^11 / 31) ^ 22) + 26 < 134 + 26 = 160; */

enum LZMA_REQUIRED_INPUT_MAX = 20;

public struct CLzmaDec {
private:
  CLzmaProps prop;
  CLzmaProb* probs;
  public Byte* dic;
  const(Byte)* buf;
  UInt32 range, code;
  public SizeT dicPos;
  public SizeT dicBufSize;
  UInt32 processedPos;
  UInt32 checkDicSize;
  uint state;
  UInt32[4] reps;
  uint remainLen;
  int needFlush;
  int needInitState;
  UInt32 numProbs;
  uint tempBufSize;
  Byte[LZMA_REQUIRED_INPUT_MAX] tempBuf;
}

//#define LzmaDec_Construct(p) { (p).dic = 0; (p).probs = 0; }

//void LzmaDec_Init(CLzmaDec *p);

/* There are two types of LZMA streams:
     0) Stream with end mark. That end mark adds about 6 bytes to compressed size.
     1) Stream without end mark. You must know exact uncompressed size to decompress such stream. */

public alias ELzmaFinishMode = int;
public enum /*ELzmaFinishMode*/ {
  LZMA_FINISH_ANY,   /* finish at any point */
  LZMA_FINISH_END    /* block must be finished at the end */
}

/* ELzmaFinishMode has meaning only if the decoding reaches output limit !!!

   You must use LZMA_FINISH_END, when you know that current output buffer
   covers last bytes of block. In other cases you must use LZMA_FINISH_ANY.

   If LZMA decoder sees end marker before reaching output limit, it returns SRes.OK,
   and output value of destLen will be less than output buffer size limit.
   You can check status result also.

   You can use multiple checks to test data integrity after full decompression:
     1) Check Result and "status" variable.
     2) Check that output(destLen) = uncompressedSize, if you know real uncompressedSize.
     3) Check that output(srcLen) = compressedSize, if you know real compressedSize.
        You must use correct finish mode in that case. */

public alias ELzmaStatus = int;
public enum /*ELzmaStatus*/ {
  LZMA_STATUS_NOT_SPECIFIED,               /* use main error code instead */
  LZMA_STATUS_FINISHED_WITH_MARK,          /* stream was finished with end mark. */
  LZMA_STATUS_NOT_FINISHED,                /* stream was not finished */
  LZMA_STATUS_NEEDS_MORE_INPUT,            /* you must provide more input bytes */
  LZMA_STATUS_MAYBE_FINISHED_WITHOUT_MARK  /* there is probability that stream was finished without end mark */
}

/* ELzmaStatus is used only as output value for function call */


/* ---------- Interfaces ---------- */

/* There are 3 levels of interfaces:
     1) Dictionary Interface
     2) Buffer Interface
     3) One Call Interface
   You can select any of these interfaces, but don't mix functions from different
   groups for same object. */


/* There are two variants to allocate state for Dictionary Interface:
     1) LzmaDec_Allocate / LzmaDec_Free
     2) LzmaDec_AllocateProbs / LzmaDec_FreeProbs
   You can use variant 2, if you set dictionary buffer manually.
   For Buffer Interface you must always use variant 1.

LzmaDec_Allocate* can return:
  SRes.OK
  SRes.ERROR_MEM         - Memory allocation error
  SRes.ERROR_UNSUPPORTED - Unsupported properties
*/

/*
SRes LzmaDec_AllocateProbs(CLzmaDec *p, const(Byte)* props, uint propsSize);
void LzmaDec_FreeProbs(CLzmaDec *p);

SRes LzmaDec_Allocate(CLzmaDec *state, const(Byte)* prop, uint propsSize);
void LzmaDec_Free(CLzmaDec *state);
*/

/* ---------- Dictionary Interface ---------- */

/* You can use it, if you want to eliminate the overhead for data copying from
   dictionary to some other external buffer.
   You must work with CLzmaDec variables directly in this interface.

   STEPS:
     LzmaDec_Constr()
     LzmaDec_Allocate()
     for (each new stream)
     {
       LzmaDec_Init()
       while (it needs more decompression)
       {
         LzmaDec_DecodeToDic()
         use data from CLzmaDec::dic and update CLzmaDec::dicPos
       }
     }
     LzmaDec_Free()
*/

/* LzmaDec_DecodeToDic

   The decoding to internal dictionary buffer (CLzmaDec::dic).
   You must manually update CLzmaDec::dicPos, if it reaches CLzmaDec::dicBufSize !!!

finishMode:
  It has meaning only if the decoding reaches output limit (dicLimit).
  LZMA_FINISH_ANY - Decode just dicLimit bytes.
  LZMA_FINISH_END - Stream must be finished after dicLimit.

Returns:
  SRes.OK
    status:
      LZMA_STATUS_FINISHED_WITH_MARK
      LZMA_STATUS_NOT_FINISHED
      LZMA_STATUS_NEEDS_MORE_INPUT
      LZMA_STATUS_MAYBE_FINISHED_WITHOUT_MARK
  SRes.ERROR_DATA - Data error
*/

//SRes LzmaDec_DecodeToDic(CLzmaDec *p, SizeT dicLimit, const(Byte)* src, SizeT *srcLen, ELzmaFinishMode finishMode, ELzmaStatus *status);


/* ---------- Buffer Interface ---------- */

/* It's zlib-like interface.
   See LzmaDec_DecodeToDic description for information about STEPS and return results,
   but you must use LzmaDec_DecodeToBuf instead of LzmaDec_DecodeToDic and you don't need
   to work with CLzmaDec variables manually.

finishMode:
  It has meaning only if the decoding reaches output limit (*destLen).
  LZMA_FINISH_ANY - Decode just destLen bytes.
  LZMA_FINISH_END - Stream must be finished after (*destLen).
*/

//SRes LzmaDec_DecodeToBuf(CLzmaDec *p, Byte *dest, SizeT *destLen, const(Byte)* src, SizeT *srcLen, ELzmaFinishMode finishMode, ELzmaStatus *status);


/* ---------- One Call Interface ---------- */

/* LzmaDecode

finishMode:
  It has meaning only if the decoding reaches output limit (*destLen).
  LZMA_FINISH_ANY - Decode just destLen bytes.
  LZMA_FINISH_END - Stream must be finished after (*destLen).

Returns:
  SRes.OK
    status:
      LZMA_STATUS_FINISHED_WITH_MARK
      LZMA_STATUS_NOT_FINISHED
      LZMA_STATUS_MAYBE_FINISHED_WITHOUT_MARK
  SRes.ERROR_DATA - Data error
  SRes.ERROR_MEM  - Memory allocation error
  SRes.ERROR_UNSUPPORTED - Unsupported properties
  SRes.ERROR_INPUT_EOF - It needs more bytes in input buffer (src).
*/

/*
SRes LzmaDecode(Byte *dest, SizeT *destLen, const(Byte)* src, SizeT *srcLen,
    const(Byte)* propData, uint propSize, ELzmaFinishMode finishMode,
    ELzmaStatus *status, ISzAllocPtr alloc);
*/

// ////////////////////////////////////////////////////////////////////////// //
private:

enum kNumTopBits = 24;
enum kTopValue = 1U<<kNumTopBits;

enum kNumBitModelTotalBits = 11;
enum kBitModelTotal = 1<<kNumBitModelTotalBits;
enum kNumMoveBits = 5;

enum RC_INIT_SIZE = 5;

//enum NORMALIZE = "if (range < kTopValue) { range <<= 8; code = (code << 8) | (*buf++); }";

//#define IF_BIT_0(p) ttt = *(p); NORMALIZE; bound = (range >> kNumBitModelTotalBits) * ttt; if (code < bound)
//#define UPDATE_0(p) range = bound; *(p) = (CLzmaProb)(ttt + ((kBitModelTotal - ttt) >> kNumMoveBits));
//#define UPDATE_1(p) range -= bound; code -= bound; *(p) = (CLzmaProb)(ttt - (ttt >> kNumMoveBits));
enum GET_BIT2(string p, string i, string A0, string A1) =
  "ttt = *("~p~"); if (range < kTopValue) { range <<= 8; code = (code<<8)|(*buf++); } bound = (range>>kNumBitModelTotalBits)*ttt; if (code < bound)\n"~
  "{ range = bound; *("~p~") = cast(CLzmaProb)(ttt+((kBitModelTotal-ttt)>>kNumMoveBits)); "~i~" = ("~i~"+"~i~"); "~A0~" } else\n"~
  "{ range -= bound; code -= bound; *("~p~") = cast(CLzmaProb)(ttt-(ttt>>kNumMoveBits)); "~i~" = ("~i~"+"~i~")+1; "~A1~" }";
//#define GET_BIT(p, i) GET_BIT2(p, i, ; , ;)
enum GET_BIT(string p, string i) = GET_BIT2!(p, i, "", "");

enum TREE_GET_BIT(string probs, string i) = "{"~GET_BIT!("("~probs~"+"~i~")", i)~"}";

enum TREE_DECODE(string probs, string limit, string i) =
  "{ "~i~" = 1; do { "~TREE_GET_BIT!(probs, i)~" } while ("~i~" < "~limit~"); "~i~" -= "~limit~"; }";


version(_LZMA_SIZE_OPT) {
  enum TREE_6_DECODE(string probs, string i) = TREE_DECODE!(probs, "(1<<6)", i);
} else {
enum TREE_6_DECODE(string probs, string i) =
  "{ "~i~" = 1;\n"~
  TREE_GET_BIT!(probs, i)~
  TREE_GET_BIT!(probs, i)~
  TREE_GET_BIT!(probs, i)~
  TREE_GET_BIT!(probs, i)~
  TREE_GET_BIT!(probs, i)~
  TREE_GET_BIT!(probs, i)~
  i~" -= 0x40; }";
}

enum NORMAL_LITER_DEC = GET_BIT!("prob + symbol", "symbol");
enum MATCHED_LITER_DEC =
  "matchByte <<= 1;\n"~
  "bit = (matchByte & offs);\n"~
  "probLit = prob + offs + bit + symbol;\n"~
  GET_BIT2!("probLit", "symbol", "offs &= ~bit;", "offs &= bit;");

enum NORMALIZE_CHECK = "if (range < kTopValue) { if (buf >= bufLimit) return DUMMY_ERROR; range <<= 8; code = (code << 8) | (*buf++); }";

//#define IF_BIT_0_CHECK(p) ttt = *(p); NORMALIZE_CHECK; bound = (range >> kNumBitModelTotalBits) * ttt; if (code < bound)
//#define UPDATE_0_CHECK range = bound;
//#define UPDATE_1_CHECK range -= bound; code -= bound;
enum GET_BIT2_CHECK(string p, string i, string A0, string A1) =
  "ttt = *("~p~"); if (range < kTopValue) { if (buf >= bufLimit) return DUMMY_ERROR; range <<= 8; code = (code << 8) | (*buf++); } bound = (range >> kNumBitModelTotalBits) * ttt; if (code < bound)\n"~
  "{ range = bound; "~i~" = ("~i~" + "~i~"); "~A0~" } else\n"~
  "{ range -= bound; code -= bound; "~i~" = ("~i~" + "~i~") + 1; "~A1~" }";
enum GET_BIT_CHECK(string p, string i) = GET_BIT2_CHECK!(p, i, "{}", "{}");
enum TREE_DECODE_CHECK(string probs, string limit, string i) =
  "{ "~i~" = 1; do { "~GET_BIT_CHECK!(probs~"+"~i, i)~" } while ("~i~" < "~limit~"); "~i~" -= "~limit~"; }";


enum kNumPosBitsMax = 4;
enum kNumPosStatesMax = (1 << kNumPosBitsMax);

enum kLenNumLowBits = 3;
enum kLenNumLowSymbols = (1 << kLenNumLowBits);
enum kLenNumMidBits = 3;
enum kLenNumMidSymbols = (1 << kLenNumMidBits);
enum kLenNumHighBits = 8;
enum kLenNumHighSymbols = (1 << kLenNumHighBits);

enum LenChoice = 0;
enum LenChoice2 = (LenChoice + 1);
enum LenLow = (LenChoice2 + 1);
enum LenMid = (LenLow + (kNumPosStatesMax << kLenNumLowBits));
enum LenHigh = (LenMid + (kNumPosStatesMax << kLenNumMidBits));
enum kNumLenProbs = (LenHigh + kLenNumHighSymbols);


enum kNumStates = 12;
enum kNumLitStates = 7;

enum kStartPosModelIndex = 4;
enum kEndPosModelIndex = 14;
enum kNumFullDistances = (1 << (kEndPosModelIndex >> 1));

enum kNumPosSlotBits = 6;
enum kNumLenToPosStates = 4;

enum kNumAlignBits = 4;
enum kAlignTableSize = (1 << kNumAlignBits);

enum kMatchMinLen = 2;
enum kMatchSpecLenStart = (kMatchMinLen + kLenNumLowSymbols + kLenNumMidSymbols + kLenNumHighSymbols);

enum IsMatch = 0;
enum IsRep = (IsMatch + (kNumStates << kNumPosBitsMax));
enum IsRepG0 = (IsRep + kNumStates);
enum IsRepG1 = (IsRepG0 + kNumStates);
enum IsRepG2 = (IsRepG1 + kNumStates);
enum IsRep0Long = (IsRepG2 + kNumStates);
enum PosSlot = (IsRep0Long + (kNumStates << kNumPosBitsMax));
enum SpecPos = (PosSlot + (kNumLenToPosStates << kNumPosSlotBits));
enum Align = (SpecPos + kNumFullDistances - kEndPosModelIndex);
enum LenCoder = (Align + kAlignTableSize);
enum RepLenCoder = (LenCoder + kNumLenProbs);
enum Literal = (RepLenCoder + kNumLenProbs);

enum LZMA_BASE_SIZE = 1846;
enum LZMA_LIT_SIZE = 0x300;

static assert(Literal == LZMA_BASE_SIZE);

//#define LzmaProps_GetNumProbs(p) (Literal + ((UInt32)LZMA_LIT_SIZE << ((p).lc + (p).lp)))

enum LZMA_DIC_MIN = (1 << 12);

/* First LZMA-symbol is always decoded.
And it decodes new LZMA-symbols while (buf < bufLimit), but "buf" is without last normalization
Out:
  Result:
    SRes.OK - OK
    SRes.ERROR_DATA - Error
  p->remainLen:
    < kMatchSpecLenStart : normal remain
    = kMatchSpecLenStart : finished
    = kMatchSpecLenStart + 1 : Flush marker (unused now)
    = kMatchSpecLenStart + 2 : State Init Marker (unused now)
*/

private SRes LzmaDec_DecodeReal (CLzmaDec* p, SizeT limit, const(Byte)* bufLimit) {
  CLzmaProb* probs = p.probs;

  uint state = p.state;
  UInt32 rep0 = p.reps.ptr[0], rep1 = p.reps.ptr[1], rep2 = p.reps.ptr[2], rep3 = p.reps.ptr[3];
  uint pbMask = (1U<<(p.prop.pb))-1;
  uint lpMask = (1U<<(p.prop.lp))-1;
  uint lc = p.prop.lc;

  Byte* dic = p.dic;
  SizeT dicBufSize = p.dicBufSize;
  SizeT dicPos = p.dicPos;

  UInt32 processedPos = p.processedPos;
  UInt32 checkDicSize = p.checkDicSize;
  uint len = 0;

  const(Byte)* buf = p.buf;
  UInt32 range = p.range;
  UInt32 code = p.code;

  do {
    CLzmaProb *prob;
    UInt32 bound;
    uint ttt;
    uint posState = processedPos & pbMask;

    prob = probs + IsMatch + (state << kNumPosBitsMax) + posState;
    ttt = *(prob); if (range < kTopValue) { range <<= 8; code = (code << 8) | (*buf++); } bound = (range>>kNumBitModelTotalBits)*ttt; if (code < bound)
    {
      uint symbol;
      range = bound; *(prob) = cast(CLzmaProb)(ttt+((kBitModelTotal-ttt)>>kNumMoveBits));
      prob = probs + Literal;
      if (processedPos != 0 || checkDicSize != 0)
        prob += (cast(UInt32)LZMA_LIT_SIZE * (((processedPos & lpMask) << lc) +
            (dic[(dicPos == 0 ? dicBufSize : dicPos) - 1] >> (8 - lc))));
      processedPos++;

      if (state < kNumLitStates)
      {
        state -= (state < 4) ? state : 3;
        symbol = 1;
        version(_LZMA_SIZE_OPT) {
          do { mixin(NORMAL_LITER_DEC); } while (symbol < 0x100);
        } else {
          mixin(NORMAL_LITER_DEC);
          mixin(NORMAL_LITER_DEC);
          mixin(NORMAL_LITER_DEC);
          mixin(NORMAL_LITER_DEC);
          mixin(NORMAL_LITER_DEC);
          mixin(NORMAL_LITER_DEC);
          mixin(NORMAL_LITER_DEC);
          mixin(NORMAL_LITER_DEC);
        }
      }
      else
      {
        uint matchByte = dic[dicPos - rep0 + (dicPos < rep0 ? dicBufSize : 0)];
        uint offs = 0x100;
        state -= (state < 10) ? 3 : 6;
        symbol = 1;
        version(_LZMA_SIZE_OPT) {
          do
          {
            uint bit;
            CLzmaProb *probLit;
            mixin(MATCHED_LITER_DEC);
          }
          while (symbol < 0x100);
        } else {
          {
            uint bit;
            CLzmaProb *probLit;
            mixin(MATCHED_LITER_DEC);
            mixin(MATCHED_LITER_DEC);
            mixin(MATCHED_LITER_DEC);
            mixin(MATCHED_LITER_DEC);
            mixin(MATCHED_LITER_DEC);
            mixin(MATCHED_LITER_DEC);
            mixin(MATCHED_LITER_DEC);
            mixin(MATCHED_LITER_DEC);
          }
        }
      }

      dic[dicPos++] = cast(Byte)symbol;
      continue;
    }

    {
      range -= bound; code -= bound; *(prob) = cast(CLzmaProb)(ttt-(ttt>>kNumMoveBits));
      prob = probs + IsRep + state;
      ttt = *(prob); if (range < kTopValue) { range <<= 8; code = (code << 8) | (*buf++); } bound = (range>>kNumBitModelTotalBits)*ttt; if (code < bound)
      {
        range = bound; *(prob) = cast(CLzmaProb)(ttt+((kBitModelTotal-ttt)>>kNumMoveBits));
        state += kNumStates;
        prob = probs + LenCoder;
      }
      else
      {
        range -= bound; code -= bound; *(prob) = cast(CLzmaProb)(ttt-(ttt>>kNumMoveBits));
        if (checkDicSize == 0 && processedPos == 0)
          return SRes.ERROR_DATA;
        prob = probs + IsRepG0 + state;
        ttt = *(prob); if (range < kTopValue) { range <<= 8; code = (code << 8) | (*buf++); } bound = (range>>kNumBitModelTotalBits)*ttt; if (code < bound)
        {
          range = bound; *(prob) = cast(CLzmaProb)(ttt+((kBitModelTotal-ttt)>>kNumMoveBits));
          prob = probs + IsRep0Long + (state << kNumPosBitsMax) + posState;
          ttt = *(prob); if (range < kTopValue) { range <<= 8; code = (code << 8) | (*buf++); } bound = (range>>kNumBitModelTotalBits)*ttt; if (code < bound)
          {
            range = bound; *(prob) = cast(CLzmaProb)(ttt+((kBitModelTotal-ttt)>>kNumMoveBits));
            dic[dicPos] = dic[dicPos - rep0 + (dicPos < rep0 ? dicBufSize : 0)];
            dicPos++;
            processedPos++;
            state = state < kNumLitStates ? 9 : 11;
            continue;
          }
          range -= bound; code -= bound; *(prob) = cast(CLzmaProb)(ttt-(ttt>>kNumMoveBits));
        }
        else
        {
          UInt32 distance;
          range -= bound; code -= bound; *(prob) = cast(CLzmaProb)(ttt-(ttt>>kNumMoveBits));
          prob = probs + IsRepG1 + state;
          ttt = *(prob); if (range < kTopValue) { range <<= 8; code = (code << 8) | (*buf++); } bound = (range>>kNumBitModelTotalBits)*ttt; if (code < bound)
          {
            range = bound; *(prob) = cast(CLzmaProb)(ttt+((kBitModelTotal-ttt)>>kNumMoveBits));
            distance = rep1;
          }
          else
          {
            range -= bound; code -= bound; *(prob) = cast(CLzmaProb)(ttt-(ttt>>kNumMoveBits));
            prob = probs + IsRepG2 + state;
            ttt = *(prob); if (range < kTopValue) { range <<= 8; code = (code << 8) | (*buf++); } bound = (range>>kNumBitModelTotalBits)*ttt; if (code < bound)
            {
              range = bound; *(prob) = cast(CLzmaProb)(ttt+((kBitModelTotal-ttt)>>kNumMoveBits));
              distance = rep2;
            }
            else
            {
              range -= bound; code -= bound; *(prob) = cast(CLzmaProb)(ttt-(ttt>>kNumMoveBits));
              distance = rep3;
              rep3 = rep2;
            }
            rep2 = rep1;
          }
          rep1 = rep0;
          rep0 = distance;
        }
        state = state < kNumLitStates ? 8 : 11;
        prob = probs + RepLenCoder;
      }

      version(_LZMA_SIZE_OPT) {
        {
          uint lim, offset;
          CLzmaProb *probLen = prob + LenChoice;
          ttt = *(probLen); if (range < kTopValue) { range <<= 8; code = (code << 8) | (*buf++); } bound = (range>>kNumBitModelTotalBits)*ttt; if (code < bound)
          {
            range = bound; *(probLen) = cast(CLzmaProb)(ttt+((kBitModelTotal-ttt)>>kNumMoveBits));
            probLen = prob + LenLow + (posState << kLenNumLowBits);
            offset = 0;
            lim = (1 << kLenNumLowBits);
          }
          else
          {
            range -= bound; code -= bound; *(probLen) = cast(CLzmaProb)(ttt-(ttt>>kNumMoveBits));
            probLen = prob + LenChoice2;
            ttt = *(probLen); if (range < kTopValue) { range <<= 8; code = (code << 8) | (*buf++); } bound = (range>>kNumBitModelTotalBits)*ttt; if (code < bound)
            {
              range = bound; *(probLen) = cast(CLzmaProb)(ttt+((kBitModelTotal-ttt)>>kNumMoveBits));
              probLen = prob + LenMid + (posState << kLenNumMidBits);
              offset = kLenNumLowSymbols;
              lim = (1 << kLenNumMidBits);
            }
            else
            {
              range -= bound; code -= bound; *(probLen) = cast(CLzmaProb)(ttt-(ttt>>kNumMoveBits));
              probLen = prob + LenHigh;
              offset = kLenNumLowSymbols + kLenNumMidSymbols;
              lim = (1 << kLenNumHighBits);
            }
          }
          mixin(TREE_DECODE!("probLen", "lim", "len"));
          len += offset;
        }
      } else {
        {
          CLzmaProb *probLen = prob + LenChoice;
          ttt = *(probLen); if (range < kTopValue) { range <<= 8; code = (code << 8) | (*buf++); } bound = (range>>kNumBitModelTotalBits)*ttt; if (code < bound)
          {
            range = bound; *(probLen) = cast(CLzmaProb)(ttt+((kBitModelTotal-ttt)>>kNumMoveBits));
            probLen = prob + LenLow + (posState << kLenNumLowBits);
            len = 1;
            mixin(TREE_GET_BIT!("probLen", "len"));
            mixin(TREE_GET_BIT!("probLen", "len"));
            mixin(TREE_GET_BIT!("probLen", "len"));
            len -= 8;
          }
          else
          {
            range -= bound; code -= bound; *(probLen) = cast(CLzmaProb)(ttt-(ttt>>kNumMoveBits));
            probLen = prob + LenChoice2;
            ttt = *(probLen); if (range < kTopValue) { range <<= 8; code = (code << 8) | (*buf++); } bound = (range>>kNumBitModelTotalBits)*ttt; if (code < bound)
            {
              range = bound; *(probLen) = cast(CLzmaProb)(ttt+((kBitModelTotal-ttt)>>kNumMoveBits));
              probLen = prob + LenMid + (posState << kLenNumMidBits);
              len = 1;
              mixin(TREE_GET_BIT!("probLen", "len"));
              mixin(TREE_GET_BIT!("probLen", "len"));
              mixin(TREE_GET_BIT!("probLen", "len"));
            }
            else
            {
              range -= bound; code -= bound; *(probLen) = cast(CLzmaProb)(ttt-(ttt>>kNumMoveBits));
              probLen = prob + LenHigh;
              mixin(TREE_DECODE!("probLen", "(1 << kLenNumHighBits)", "len"));
              len += kLenNumLowSymbols + kLenNumMidSymbols;
            }
          }
        }
      }

      if (state >= kNumStates)
      {
        UInt32 distance;
        prob = probs + PosSlot +
            ((len < kNumLenToPosStates ? len : kNumLenToPosStates - 1) << kNumPosSlotBits);
        mixin(TREE_6_DECODE!("prob", "distance"));
        if (distance >= kStartPosModelIndex)
        {
          uint posSlot = cast(uint)distance;
          uint numDirectBits = cast(uint)(((distance >> 1) - 1));
          distance = (2 | (distance & 1));
          if (posSlot < kEndPosModelIndex)
          {
            distance <<= numDirectBits;
            prob = probs + SpecPos + distance - posSlot - 1;
            {
              UInt32 mask = 1;
              uint i = 1;
              do
              {
                mixin(GET_BIT2!("prob + i", "i", "{}" , "distance |= mask;"));
                mask <<= 1;
              }
              while (--numDirectBits != 0);
            }
          }
          else
          {
            numDirectBits -= kNumAlignBits;
            do
            {
              if (range < kTopValue) { range <<= 8; code = (code << 8) | (*buf++); }
              range >>= 1;

              {
                UInt32 t;
                code -= range;
                t = (0 - (cast(UInt32)code >> 31)); /* (UInt32)((Int32)code >> 31) */
                distance = (distance << 1) + (t + 1);
                code += range & t;
              }
              /*
              distance <<= 1;
              if (code >= range)
              {
                code -= range;
                distance |= 1;
              }
              */
            }
            while (--numDirectBits != 0);
            prob = probs + Align;
            distance <<= kNumAlignBits;
            {
              uint i = 1;
              mixin(GET_BIT2!("prob + i", "i", "", "distance |= 1;"));
              mixin(GET_BIT2!("prob + i", "i", "", "distance |= 2;"));
              mixin(GET_BIT2!("prob + i", "i", "", "distance |= 4;"));
              mixin(GET_BIT2!("prob + i", "i", "", "distance |= 8;"));
            }
            if (distance == cast(UInt32)0xFFFFFFFF)
            {
              len += kMatchSpecLenStart;
              state -= kNumStates;
              break;
            }
          }
        }

        rep3 = rep2;
        rep2 = rep1;
        rep1 = rep0;
        rep0 = distance + 1;
        if (checkDicSize == 0)
        {
          if (distance >= processedPos)
          {
            p.dicPos = dicPos;
            return SRes.ERROR_DATA;
          }
        }
        else if (distance >= checkDicSize)
        {
          p.dicPos = dicPos;
          return SRes.ERROR_DATA;
        }
        state = (state < kNumStates + kNumLitStates) ? kNumLitStates : kNumLitStates + 3;
      }

      len += kMatchMinLen;

      {
        SizeT rem;
        uint curLen;
        SizeT pos;

        if ((rem = limit - dicPos) == 0)
        {
          p.dicPos = dicPos;
          return SRes.ERROR_DATA;
        }

        curLen = ((rem < len) ? cast(uint)rem : len);
        pos = dicPos - rep0 + (dicPos < rep0 ? dicBufSize : 0);

        processedPos += curLen;

        len -= curLen;
        if (curLen <= dicBufSize - pos)
        {
          Byte *dest = dic + dicPos;
          ptrdiff_t src = cast(ptrdiff_t)pos - cast(ptrdiff_t)dicPos;
          const(Byte)* lim = dest + curLen;
          dicPos += curLen;
          do
            *(dest) = cast(Byte)*(dest + src);
          while (++dest != lim);
        }
        else
        {
          do
          {
            dic[dicPos++] = dic[pos];
            if (++pos == dicBufSize)
              pos = 0;
          }
          while (--curLen != 0);
        }
      }
    }
  }
  while (dicPos < limit && buf < bufLimit);

  if (range < kTopValue) { range <<= 8; code = (code << 8) | (*buf++); }

  p.buf = buf;
  p.range = range;
  p.code = code;
  p.remainLen = len;
  p.dicPos = dicPos;
  p.processedPos = processedPos;
  p.reps.ptr[0] = rep0;
  p.reps.ptr[1] = rep1;
  p.reps.ptr[2] = rep2;
  p.reps.ptr[3] = rep3;
  p.state = state;

  return SRes.OK;
}

private void LzmaDec_WriteRem (CLzmaDec* p, SizeT limit) {
  if (p.remainLen != 0 && p.remainLen < kMatchSpecLenStart)
  {
    Byte *dic = p.dic;
    SizeT dicPos = p.dicPos;
    SizeT dicBufSize = p.dicBufSize;
    uint len = p.remainLen;
    SizeT rep0 = p.reps.ptr[0]; /* we use SizeT to avoid the BUG of VC14 for AMD64 */
    SizeT rem = limit - dicPos;
    if (rem < len)
      len = cast(uint)(rem);

    if (p.checkDicSize == 0 && p.prop.dicSize - p.processedPos <= len)
      p.checkDicSize = p.prop.dicSize;

    p.processedPos += len;
    p.remainLen -= len;
    while (len != 0)
    {
      len--;
      dic[dicPos] = dic[dicPos - rep0 + (dicPos < rep0 ? dicBufSize : 0)];
      dicPos++;
    }
    p.dicPos = dicPos;
  }
}

private SRes LzmaDec_DecodeReal2(CLzmaDec *p, SizeT limit, const(Byte)* bufLimit)
{
  do
  {
    SizeT limit2 = limit;
    if (p.checkDicSize == 0)
    {
      UInt32 rem = p.prop.dicSize - p.processedPos;
      if (limit - p.dicPos > rem)
        limit2 = p.dicPos + rem;
    }

    if (auto sres = LzmaDec_DecodeReal(p, limit2, bufLimit)) return sres;

    if (p.checkDicSize == 0 && p.processedPos >= p.prop.dicSize)
      p.checkDicSize = p.prop.dicSize;

    LzmaDec_WriteRem(p, limit);
  }
  while (p.dicPos < limit && p.buf < bufLimit && p.remainLen < kMatchSpecLenStart);

  if (p.remainLen > kMatchSpecLenStart)
    p.remainLen = kMatchSpecLenStart;

  return SRes.OK;
}

alias ELzmaDummy = int;
enum /*ELzmaDummy*/ {
  DUMMY_ERROR, /* unexpected end of input stream */
  DUMMY_LIT,
  DUMMY_MATCH,
  DUMMY_REP
}

private ELzmaDummy LzmaDec_TryDummy(const(CLzmaDec)* p, const(Byte)* buf, SizeT inSize)
{
  UInt32 range = p.range;
  UInt32 code = p.code;
  const(Byte)* bufLimit = buf + inSize;
  const(CLzmaProb)* probs = p.probs;
  uint state = p.state;
  ELzmaDummy res;

  {
    const(CLzmaProb)* prob;
    UInt32 bound;
    uint ttt;
    uint posState = (p.processedPos) & ((1 << p.prop.pb) - 1);

    prob = probs + IsMatch + (state << kNumPosBitsMax) + posState;
    ttt = *(prob); if (range < kTopValue) { if (buf >= bufLimit) return DUMMY_ERROR; range <<= 8; code = (code << 8) | (*buf++); } bound = (range >> kNumBitModelTotalBits) * ttt; if (code < bound)
    {
      range = bound;

      /* if (bufLimit - buf >= 7) return DUMMY_LIT; */

      prob = probs + Literal;
      if (p.checkDicSize != 0 || p.processedPos != 0)
        prob += (cast(UInt32)LZMA_LIT_SIZE *
            ((((p.processedPos) & ((1 << (p.prop.lp)) - 1)) << p.prop.lc) +
            (p.dic[(p.dicPos == 0 ? p.dicBufSize : p.dicPos) - 1] >> (8 - p.prop.lc))));

      if (state < kNumLitStates)
      {
        uint symbol = 1;
        do { mixin(GET_BIT_CHECK!("prob + symbol", "symbol")); } while (symbol < 0x100);
      }
      else
      {
        uint matchByte = p.dic[p.dicPos - p.reps.ptr[0] +
            (p.dicPos < p.reps.ptr[0] ? p.dicBufSize : 0)];
        uint offs = 0x100;
        uint symbol = 1;
        do
        {
          uint bit;
          const(CLzmaProb)* probLit;
          matchByte <<= 1;
          bit = (matchByte & offs);
          probLit = prob + offs + bit + symbol;
          mixin(GET_BIT2_CHECK!("probLit", "symbol", "offs &= ~bit;", "offs &= bit;"));
        }
        while (symbol < 0x100);
      }
      res = DUMMY_LIT;
    }
    else
    {
      uint len;
      range -= bound; code -= bound;

      prob = probs + IsRep + state;
      ttt = *(prob); if (range < kTopValue) { if (buf >= bufLimit) return DUMMY_ERROR; range <<= 8; code = (code << 8) | (*buf++); } bound = (range >> kNumBitModelTotalBits) * ttt; if (code < bound)
      {
        range = bound;
        state = 0;
        prob = probs + LenCoder;
        res = DUMMY_MATCH;
      }
      else
      {
        range -= bound; code -= bound;
        res = DUMMY_REP;
        prob = probs + IsRepG0 + state;
        ttt = *(prob); if (range < kTopValue) { if (buf >= bufLimit) return DUMMY_ERROR; range <<= 8; code = (code << 8) | (*buf++); } bound = (range >> kNumBitModelTotalBits) * ttt; if (code < bound)
        {
          range = bound;
          prob = probs + IsRep0Long + (state << kNumPosBitsMax) + posState;
          ttt = *(prob); if (range < kTopValue) { if (buf >= bufLimit) return DUMMY_ERROR; range <<= 8; code = (code << 8) | (*buf++); } bound = (range >> kNumBitModelTotalBits) * ttt; if (code < bound)
          {
            range = bound;
            if (range < kTopValue) { if (buf >= bufLimit) return DUMMY_ERROR; range <<= 8; code = (code << 8) | (*buf++); }
            return DUMMY_REP;
          }
          else
          {
            range -= bound; code -= bound;
          }
        }
        else
        {
          range -= bound; code -= bound;
          prob = probs + IsRepG1 + state;
          ttt = *(prob); if (range < kTopValue) { if (buf >= bufLimit) return DUMMY_ERROR; range <<= 8; code = (code << 8) | (*buf++); } bound = (range >> kNumBitModelTotalBits) * ttt; if (code < bound)
          {
            range = bound;
          }
          else
          {
            range -= bound; code -= bound;
            prob = probs + IsRepG2 + state;
            ttt = *(prob); if (range < kTopValue) { if (buf >= bufLimit) return DUMMY_ERROR; range <<= 8; code = (code << 8) | (*buf++); } bound = (range >> kNumBitModelTotalBits) * ttt; if (code < bound)
            {
              range = bound;
            }
            else
            {
              range -= bound; code -= bound;
            }
          }
        }
        state = kNumStates;
        prob = probs + RepLenCoder;
      }
      {
        uint limit, offset;
        const(CLzmaProb)* probLen = prob + LenChoice;
        ttt = *(probLen); if (range < kTopValue) { if (buf >= bufLimit) return DUMMY_ERROR; range <<= 8; code = (code << 8) | (*buf++); } bound = (range >> kNumBitModelTotalBits) * ttt; if (code < bound)
        {
          range = bound;
          probLen = prob + LenLow + (posState << kLenNumLowBits);
          offset = 0;
          limit = 1 << kLenNumLowBits;
        }
        else
        {
          range -= bound; code -= bound;
          probLen = prob + LenChoice2;
          ttt = *(probLen); if (range < kTopValue) { if (buf >= bufLimit) return DUMMY_ERROR; range <<= 8; code = (code << 8) | (*buf++); } bound = (range >> kNumBitModelTotalBits) * ttt; if (code < bound)
          {
            range = bound;
            probLen = prob + LenMid + (posState << kLenNumMidBits);
            offset = kLenNumLowSymbols;
            limit = 1 << kLenNumMidBits;
          }
          else
          {
            range -= bound; code -= bound;
            probLen = prob + LenHigh;
            offset = kLenNumLowSymbols + kLenNumMidSymbols;
            limit = 1 << kLenNumHighBits;
          }
        }
        mixin(TREE_DECODE_CHECK!("probLen", "limit", "len"));
        len += offset;
      }

      if (state < 4)
      {
        uint posSlot;
        prob = probs + PosSlot +
            ((len < kNumLenToPosStates ? len : kNumLenToPosStates - 1) <<
            kNumPosSlotBits);
        mixin(TREE_DECODE_CHECK!("prob", "1 << kNumPosSlotBits", "posSlot"));
        if (posSlot >= kStartPosModelIndex)
        {
          uint numDirectBits = ((posSlot >> 1) - 1);

          /* if (bufLimit - buf >= 8) return DUMMY_MATCH; */

          if (posSlot < kEndPosModelIndex)
          {
            prob = probs + SpecPos + ((2 | (posSlot & 1)) << numDirectBits) - posSlot - 1;
          }
          else
          {
            numDirectBits -= kNumAlignBits;
            do
            {
              if (range < kTopValue) { if (buf >= bufLimit) return DUMMY_ERROR; range <<= 8; code = (code << 8) | (*buf++); }
              range >>= 1;
              code -= range & (((code - range) >> 31) - 1);
              /* if (code >= range) code -= range; */
            }
            while (--numDirectBits != 0);
            prob = probs + Align;
            numDirectBits = kNumAlignBits;
          }
          {
            uint i = 1;
            do
            {
              mixin(GET_BIT_CHECK!("prob + i", "i"));
            }
            while (--numDirectBits != 0);
          }
        }
      }
    }
  }
  if (range < kTopValue) { if (buf >= bufLimit) return DUMMY_ERROR; range <<= 8; code = (code << 8) | (*buf++); }
  return res;
}


void LzmaDec_InitDicAndState(CLzmaDec *p, bool initDic, bool initState)
{
  p.needFlush = 1;
  p.remainLen = 0;
  p.tempBufSize = 0;

  if (initDic)
  {
    p.processedPos = 0;
    p.checkDicSize = 0;
    p.needInitState = 1;
  }
  if (initState)
    p.needInitState = 1;
}

public void LzmaDec_Init(CLzmaDec *p)
{
  p.dicPos = 0;
  LzmaDec_InitDicAndState(p, true, true);
}

private void LzmaDec_InitStateReal(CLzmaDec *p)
{
  SizeT numProbs = (Literal+(cast(UInt32)LZMA_LIT_SIZE<<((&p.prop).lc+(&p.prop).lp)));
  SizeT i;
  CLzmaProb *probs = p.probs;
  for (i = 0; i < numProbs; i++)
    probs[i] = kBitModelTotal >> 1;
  p.reps.ptr[0] = p.reps.ptr[1] = p.reps.ptr[2] = p.reps.ptr[3] = 1;
  p.state = 0;
  p.needInitState = 0;
}

public SRes LzmaDec_DecodeToDic(CLzmaDec *p, SizeT dicLimit, const(Byte)* src, SizeT *srcLen,
    ELzmaFinishMode finishMode, ELzmaStatus *status)
{
  SizeT inSize = *srcLen;
  (*srcLen) = 0;
  LzmaDec_WriteRem(p, dicLimit);

  *status = LZMA_STATUS_NOT_SPECIFIED;

  while (p.remainLen != kMatchSpecLenStart)
  {
      int checkEndMarkNow;

      if (p.needFlush)
      {
        for (; inSize > 0 && p.tempBufSize < RC_INIT_SIZE; (*srcLen)++, inSize--)
          p.tempBuf.ptr[p.tempBufSize++] = *src++;
        if (p.tempBufSize < RC_INIT_SIZE)
        {
          *status = LZMA_STATUS_NEEDS_MORE_INPUT;
          return SRes.OK;
        }
        if (p.tempBuf.ptr[0] != 0)
          return SRes.ERROR_DATA;
        p.code =
              (cast(UInt32)p.tempBuf.ptr[1] << 24)
            | (cast(UInt32)p.tempBuf.ptr[2] << 16)
            | (cast(UInt32)p.tempBuf.ptr[3] << 8)
            | (cast(UInt32)p.tempBuf.ptr[4]);
        p.range = 0xFFFFFFFF;
        p.needFlush = 0;
        p.tempBufSize = 0;
      }

      checkEndMarkNow = 0;
      if (p.dicPos >= dicLimit)
      {
        if (p.remainLen == 0 && p.code == 0)
        {
          *status = LZMA_STATUS_MAYBE_FINISHED_WITHOUT_MARK;
          return SRes.OK;
        }
        if (finishMode == LZMA_FINISH_ANY)
        {
          *status = LZMA_STATUS_NOT_FINISHED;
          return SRes.OK;
        }
        if (p.remainLen != 0)
        {
          *status = LZMA_STATUS_NOT_FINISHED;
          return SRes.ERROR_DATA;
        }
        checkEndMarkNow = 1;
      }

      if (p.needInitState)
        LzmaDec_InitStateReal(p);

      if (p.tempBufSize == 0)
      {
        SizeT processed;
        const(Byte)* bufLimit;
        if (inSize < LZMA_REQUIRED_INPUT_MAX || checkEndMarkNow)
        {
          int dummyRes = LzmaDec_TryDummy(p, src, inSize);
          if (dummyRes == DUMMY_ERROR)
          {
            import core.stdc.string : memcpy;
            memcpy(p.tempBuf.ptr, src, inSize);
            p.tempBufSize = cast(uint)inSize;
            (*srcLen) += inSize;
            *status = LZMA_STATUS_NEEDS_MORE_INPUT;
            return SRes.OK;
          }
          if (checkEndMarkNow && dummyRes != DUMMY_MATCH)
          {
            *status = LZMA_STATUS_NOT_FINISHED;
            return SRes.ERROR_DATA;
          }
          bufLimit = src;
        }
        else
          bufLimit = src + inSize - LZMA_REQUIRED_INPUT_MAX;
        p.buf = src;
        if (LzmaDec_DecodeReal2(p, dicLimit, bufLimit) != 0)
          return SRes.ERROR_DATA;
        processed = cast(SizeT)(p.buf - src);
        (*srcLen) += processed;
        src += processed;
        inSize -= processed;
      }
      else
      {
        uint rem = p.tempBufSize, lookAhead = 0;
        while (rem < LZMA_REQUIRED_INPUT_MAX && lookAhead < inSize)
          p.tempBuf.ptr[rem++] = src[lookAhead++];
        p.tempBufSize = rem;
        if (rem < LZMA_REQUIRED_INPUT_MAX || checkEndMarkNow)
        {
          int dummyRes = LzmaDec_TryDummy(p, p.tempBuf.ptr, rem);
          if (dummyRes == DUMMY_ERROR)
          {
            (*srcLen) += lookAhead;
            *status = LZMA_STATUS_NEEDS_MORE_INPUT;
            return SRes.OK;
          }
          if (checkEndMarkNow && dummyRes != DUMMY_MATCH)
          {
            *status = LZMA_STATUS_NOT_FINISHED;
            return SRes.ERROR_DATA;
          }
        }
        p.buf = p.tempBuf.ptr;
        if (LzmaDec_DecodeReal2(p, dicLimit, p.buf) != 0)
          return SRes.ERROR_DATA;

        {
          uint kkk = cast(uint)(p.buf - p.tempBuf.ptr);
          if (rem < kkk)
            return SRes.ERROR_FAIL; /* some internal error */
          rem -= kkk;
          if (lookAhead < rem)
            return SRes.ERROR_FAIL; /* some internal error */
          lookAhead -= rem;
        }
        (*srcLen) += lookAhead;
        src += lookAhead;
        inSize -= lookAhead;
        p.tempBufSize = 0;
      }
  }
  if (p.code == 0)
    *status = LZMA_STATUS_FINISHED_WITH_MARK;
  return (p.code == 0) ? SRes.OK : SRes.ERROR_DATA;
}

public SRes LzmaDec_DecodeToBuf(CLzmaDec *p, Byte *dest, SizeT *destLen, const(Byte)* src, SizeT *srcLen, ELzmaFinishMode finishMode, ELzmaStatus *status)
{
  import core.stdc.string : memcpy;
  SizeT outSize = *destLen;
  SizeT inSize = *srcLen;
  *srcLen = *destLen = 0;
  for (;;)
  {
    SizeT inSizeCur = inSize, outSizeCur, dicPos;
    ELzmaFinishMode curFinishMode;
    SRes res;
    if (p.dicPos == p.dicBufSize)
      p.dicPos = 0;
    dicPos = p.dicPos;
    if (outSize > p.dicBufSize - dicPos)
    {
      outSizeCur = p.dicBufSize;
      curFinishMode = LZMA_FINISH_ANY;
    }
    else
    {
      outSizeCur = dicPos + outSize;
      curFinishMode = finishMode;
    }

    res = LzmaDec_DecodeToDic(p, outSizeCur, src, &inSizeCur, curFinishMode, status);
    src += inSizeCur;
    inSize -= inSizeCur;
    *srcLen += inSizeCur;
    outSizeCur = p.dicPos - dicPos;
    memcpy(dest, p.dic + dicPos, outSizeCur);
    dest += outSizeCur;
    outSize -= outSizeCur;
    *destLen += outSizeCur;
    if (res != 0)
      return res;
    if (outSizeCur == 0 || outSize == 0)
      return SRes.OK;
  }
}

public void LzmaDec_FreeProbs(CLzmaDec *p) {
  import core.stdc.stdlib : free;
  if (p.probs !is null) free(p.probs);
  p.probs = null;
}

private void LzmaDec_FreeDict(CLzmaDec *p) {
  import core.stdc.stdlib : free;
  if (p.dic !is null) free(p.dic);
  p.dic = null;
}

public void LzmaDec_Free(CLzmaDec *p) {
  LzmaDec_FreeProbs(p);
  LzmaDec_FreeDict(p);
}

public SRes LzmaProps_Decode(CLzmaProps *p, const(Byte)*data, uint size)
{
  UInt32 dicSize;
  Byte d;

  if (size < LZMA_PROPS_SIZE)
    return SRes.ERROR_UNSUPPORTED;
  else
    dicSize = data[1] | (data[2] << 8) | (data[3] << 16) | (data[4] << 24);

  if (dicSize < LZMA_DIC_MIN)
    dicSize = LZMA_DIC_MIN;
  p.dicSize = dicSize;

  d = data[0];
  if (d >= (9 * 5 * 5))
    return SRes.ERROR_UNSUPPORTED;

  p.lc = d % 9;
  d /= 9;
  p.pb = d / 5;
  p.lp = d % 5;

  return SRes.OK;
}

private SRes LzmaDec_AllocateProbs2(CLzmaDec *p, const(CLzmaProps)* propNew) {
  import core.stdc.stdlib : malloc;
  UInt32 numProbs = (Literal+(cast(UInt32)LZMA_LIT_SIZE<<((propNew).lc+(propNew).lp)));
  if (!p.probs || numProbs != p.numProbs)
  {
    LzmaDec_FreeProbs(p);
    p.probs = cast(CLzmaProb *)malloc(numProbs * CLzmaProb.sizeof);
    p.numProbs = numProbs;
    if (!p.probs)
      return SRes.ERROR_MEM;
  }
  return SRes.OK;
}

public SRes LzmaDec_AllocateProbs(CLzmaDec *p, const(Byte)* props, uint propsSize)
{
  CLzmaProps propNew;
  if (auto sres = LzmaProps_Decode(&propNew, props, propsSize)) return sres;
  if (auto sres = LzmaDec_AllocateProbs2(p, &propNew)) return sres;
  p.prop = propNew;
  return SRes.OK;
}

public SRes LzmaDec_Allocate(CLzmaDec *p, const(Byte)*props, uint propsSize)
{
  import core.stdc.stdlib : malloc;
  CLzmaProps propNew;
  SizeT dicBufSize;
  if (auto sres = LzmaProps_Decode(&propNew, props, propsSize)) return sres;
  if (auto sres = LzmaDec_AllocateProbs2(p, &propNew)) return sres;

  {
    UInt32 dictSize = propNew.dicSize;
    SizeT mask = (1U << 12) - 1;
         if (dictSize >= (1U << 30)) mask = (1U << 22) - 1;
    else if (dictSize >= (1U << 22)) mask = (1U << 20) - 1;
    dicBufSize = (cast(SizeT)dictSize + mask) & ~mask;
    if (dicBufSize < dictSize)
      dicBufSize = dictSize;
  }

  if (!p.dic || dicBufSize != p.dicBufSize)
  {
    LzmaDec_FreeDict(p);
    p.dic = cast(Byte *)malloc(dicBufSize);
    if (!p.dic)
    {
      LzmaDec_FreeProbs(p);
      return SRes.ERROR_MEM;
    }
  }
  p.dicBufSize = dicBufSize;
  p.prop = propNew;
  return SRes.OK;
}

public SRes LzmaDecode(Byte *dest, SizeT *destLen, const(Byte)* src, SizeT *srcLen,
    const(Byte)* propData, uint propSize, ELzmaFinishMode finishMode,
    ELzmaStatus *status)
{
  CLzmaDec p;
  SRes res;
  SizeT outSize = *destLen, inSize = *srcLen;
  *destLen = *srcLen = 0;
  *status = LZMA_STATUS_NOT_SPECIFIED;
  if (inSize < RC_INIT_SIZE)
    return SRes.ERROR_INPUT_EOF;
  //LzmaDec_Construct(&p);
  p.dic = null; p.probs = null;
  if (auto sres = LzmaDec_AllocateProbs(&p, propData, propSize)) return sres;
  p.dic = dest;
  p.dicBufSize = outSize;
  LzmaDec_Init(&p);
  *srcLen = inSize;
  res = LzmaDec_DecodeToDic(&p, outSize, src, srcLen, finishMode, status);
  *destLen = p.dicPos;
  if (res == SRes.OK && *status == LZMA_STATUS_NEEDS_MORE_INPUT)
    res = SRes.ERROR_INPUT_EOF;
  LzmaDec_FreeProbs(&p);
  return res;
}
