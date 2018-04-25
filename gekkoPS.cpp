/**************************************************************************************************
*
*
*	Nintendo GameCube Gekko CPU Extension Module
*
*	The PowerPC processor module in IDA Pro does not handle Gekko Paired Single instructions.
*	Fortunately IDA Pro supports the concept of extension modules that can add support for
*	non-standard instructions, so this extension adds support for the PS instruction set.
*
*
*	INSTALLATION
*	------------
*
*	Place the processor extension modules (gekkoPS.plw) within your IDA Pro 'plugins' directory.
*	By default the plugin is active when dealing with PPC code, but you can disable/re-enable the
*	plugin by using the entry in the Edit/Plugins menu. If you want the plugin to be disabled on
*	load, you will have to edit this source code. Change the value of g_HookState to 'kDisabled'
*	and rebuild.
*
*
*	CHANGES
*	-------
*
*	2007.12.22	HyperIris	V1.0	Created, base on Dean's PowerPC Altivec/VMX Extension Module
*									Support IDA Pro 5.2
*
*
***************************************************************************************************/

#define	GEKKO_VERSION	"2018.04.25"

// SDK 4.8's "pro.h" (line 718) has a signed/unsigned mismatch, so we disable this warning..
//#pragma warning( disable:4018 )
#pragma warning( disable:4244 )

#include <ida.hpp>
#include <idp.hpp>
#include <name.hpp>
#include <bytes.hpp>
#include <loader.hpp>
#include <kernwin.hpp>

/***************************************************************************************************
*
*	Data needed to maintain plugin state
*
***************************************************************************************************/

enum HookState
{
	kDefault,
	kEnabled,
	kDisabled,
};

static HookState	g_HookState = kEnabled;
static netnode		g_GekkoNode;
static const char	g_GekkoNodeName[] = "$ PowerPC Gekko Extension Parameters";

// -------------------------------------------------------------------------------------------------
// Macros used to define opcode table

#define OP(x)				((((unsigned long)(x)) & 0x3f) << 26)
#define OP_MASK				OP (0x3f)

#define OPS(op, xop)		(OP (op) | ((((unsigned long)(xop)) & 0x1f) << 1))
#define OPSC(op, xop, rc)	(OPS ((op), (xop)) | ((rc) & 1))
//#define OPSC(op, xop, rc)	(OPS ((op), (xop)) | rc)
#define OPS_MASK			OPSC (0x3f, 0x1f, 1)
#define OPS_MASK_DOT		OPSC (0x3f, 0x1f, 1)

#define OPM(op, xop)		(OP (op) | ((((unsigned long)(xop)) & 0x3f) << 1))
#define OPMC(op, xop, rc)	(OPM ((op), (xop)) | ((rc) & 1))
#define OPM_MASK			OPMC (0x3f, 0x3f, 0)

#define OPL(op, xop)		(OP (op) | ((((unsigned long)(xop)) & 0x3ff) << 1))
#define OPLC(op, xop, rc)	(OPL ((op), (xop)) | ((rc) & 1))
//#define OPLC(op, xop, rc)	(OPL ((op), (xop)) | rc)
#define OPL_MASK			OPLC (0x3f, 0x3ff, 1)
#define OPL_MASK_DOT		OPLC (0x3f, 0x3ff, 1)


// -------------------------------------------------------------------------------------------------
// Operand identifiers (they map into g_gekkoPsOperands array)

enum	gekko_ps_operand_id
{
	NO_OPERAND,
	FA,
	FB,
	FC,
	FD,
	FS = FD,

	crfD,

	WB,
	IB,
	WC,
	IC,
	//	D,

	RA,
	RB,
	DRA,
	DRB,
};

// -------------------------------------------------------------------------------------------------
// Structure used to define an operand 

struct	gekko_ps_operand
{
	int	bits;
	int	shift;
};

gekko_ps_operand	g_gekkoPsOperands[] =
{
	{ 0, 0	},	// No Operand
	{ 5, 16	},	// FA
	{ 5, 11	},	// FB
	{ 5, 6	},	// FC
	{ 5, 21	},	// FD/FS

	{ 3, 23	},	// crfD,


	{ 1, 16	},	// WB,
	{ 3, 12	},	// IB,
	{ 1, 10	},	// WC,
	{ 3, 7	},	// IC,
//	{ 12, 0	},	// D,

	{ 5, 16	},	// RA
	{ 5, 11	},	// RB
	{ 5, 16 },	// DRA,
	{ 5, 11 },	// DRB,
};

// -------------------------------------------------------------------------------------------------
// Opcode identifiers (they map into g_gekkoPsOpcodes array)

enum gekko_ps_insn_type_t
{
	gekko_ps_insn_start = CUSTOM_INSN_ITYPE,

	gekko_psq_lx = gekko_ps_insn_start,
	gekko_psq_stx,
	gekko_psq_lux,
	gekko_psq_stux,
	gekko_psq_l,
	gekko_psq_lu,
	gekko_psq_st,
	gekko_psq_stu,

	gekko_ps_div,
	gekko_ps_div_dot,
	gekko_ps_sub,
	gekko_ps_sub_dot,
	gekko_ps_add,
	gekko_ps_add_dot,
	gekko_ps_sel,
	gekko_ps_sel_dot,
	gekko_ps_res,
	gekko_ps_res_dot,
	gekko_ps_mul,
	gekko_ps_mul_dot,
	gekko_ps_rsqrte,
	gekko_ps_rsqrte_dot,
	gekko_ps_msub,
	gekko_ps_msub_dot,
	gekko_ps_madd,
	gekko_ps_madd_dot,
	gekko_ps_nmsub,
	gekko_ps_nmsub_dot,
	gekko_ps_nmadd,
	gekko_ps_nmadd_dot,
	gekko_ps_neg,
	gekko_ps_neg_dot,
	gekko_ps_mr,
	gekko_ps_mr_dot,
	gekko_ps_nabs,
	gekko_ps_nabs_dot,
	gekko_ps_abs,
	gekko_ps_abs_dot,

	gekko_ps_sum0,
	gekko_ps_sum0_dot,
	gekko_ps_sum1,
	gekko_ps_sum1_dot,
	gekko_ps_muls0,
	gekko_ps_muls0_dot,
	gekko_ps_muls1,
	gekko_ps_muls1_dot,
	gekko_ps_madds0,
	gekko_ps_madds0_dot,
	gekko_ps_madds1,
	gekko_ps_madds1_dot,
	gekko_ps_cmpu0,
	gekko_ps_cmpo0,
	gekko_ps_cmpu1,
	gekko_ps_cmpo1,
	gekko_ps_merge00,
	gekko_ps_merge00_dot,
	gekko_ps_merge01,
	gekko_ps_merge01_dot,
	gekko_ps_merge10,
	gekko_ps_merge10_dot,
	gekko_ps_merge11,
	gekko_ps_merge11_dot,
	gekko_ps_dcbz_l,
};


// -------------------------------------------------------------------------------------------------
// Structure used to define an opcode

struct	gekko_ps_opcode
{
	gekko_ps_insn_type_t	insn;
	const char*			name;
	unsigned int		opcode;
	unsigned int		mask;
	unsigned char		operands[6];
	const char*			description;
};

gekko_ps_opcode	g_gekkoPsOpcodes[] =
{
	{	gekko_psq_lx,			"psq_lx",		OPM(4, 6),			OPM_MASK,	{ FD, RA, RB, WC, IC },	"Paired Single Quantized Load Indexed"	},
	{	gekko_psq_stx,			"psq_stx",		OPM(4, 7),			OPM_MASK,	{ FS, RA, RB, WC, IC },	"Paired Single Quantized Store Indexed"	},
	{	gekko_psq_lux,			"psq_lux",		OPM(4, 38),			OPM_MASK,	{ FD, RA, RB, WC, IC },	"Paired Single Quantized Load with update Indexed"	},
	{	gekko_psq_stux,			"psq_stux",		OPM(4, 39),			OPM_MASK,	{ FS, RA, RB, WC, IC },	"Paired Single Quantized Store with update Indexed"	},

	{	gekko_psq_l, 			"psq_l",		OP(56),				OP_MASK,	{ FD, DRA, WB, IB },	"Paired Single Quantized Load"	},
	{	gekko_psq_lu,			"psq_lu",		OP(57), 			OP_MASK,	{ FD, DRA, WB, IB },	"Paired Single Quantized Load with Update"	},
	{	gekko_psq_st,			"psq_st",		OP(60), 			OP_MASK,	{ FS, DRA, WB, IB },	"Paired Single Quantized Store"	},
	{	gekko_psq_stu,			"psq_stu",		OP(61), 			OP_MASK,	{ FS, DRA, WB, IB },	"Paired Single Quantized Store with update"	},

	{	gekko_ps_div,			"ps_div",		OPSC(4, 18, 0),		OPS_MASK,		{ FD, FA, FB},		"Paired Single Divide"	},
	{	gekko_ps_div_dot,		"ps_div.",		OPSC(4, 18, 1),		OPS_MASK_DOT,	{ FD, FA, FB},		"Paired Single Divide"	},
	{	gekko_ps_sub,			"ps_sub",		OPSC(4, 20, 0),		OPS_MASK,		{ FD, FA, FB},		"Paired Single Subtract"	},
	{	gekko_ps_sub_dot,		"ps_sub.",		OPSC(4, 20, 1),		OPS_MASK_DOT,	{ FD, FA, FB},		"Paired Single Subtract"	},
	{	gekko_ps_add,			"ps_add",		OPSC(4, 21, 0),		OPS_MASK,		{ FD, FA, FB},		"Paired Single Add"	},
	{	gekko_ps_add_dot,		"ps_add.",		OPSC(4, 21, 1),		OPS_MASK_DOT,	{ FD, FA, FB},		"Paired Single Add"	},
	{	gekko_ps_sel,			"ps_sel",		OPSC(4, 23, 0),		OPS_MASK,		{ FD, FA, FC, FB},	"Paired Single Select"	},
	{	gekko_ps_sel_dot,		"ps_sel.",		OPSC(4, 23, 1),		OPS_MASK_DOT,	{ FD, FA, FC, FB},	"Paired Single Select"	},
	{	gekko_ps_res,			"ps_res",		OPSC(4, 24, 0),		OPS_MASK,		{ FD, FB},			"Paired Single Reciprocal Estimate"	},
	{	gekko_ps_res_dot,		"ps_res.",		OPSC(4, 24, 1),		OPS_MASK_DOT,	{ FD, FB},			"Paired Single Reciprocal Estimate"	},
	{	gekko_ps_mul,			"ps_mul",		OPSC(4, 25, 0),		OPS_MASK,		{ FD, FA, FC},		"Paired Single Multiply"	},
	{	gekko_ps_mul_dot,		"ps_mul.",		OPSC(4, 25, 1),		OPS_MASK_DOT,	{ FD, FA, FC},		"Paired Single Multiply"	},
	{	gekko_ps_rsqrte,		"ps_rsqrte",	OPSC(4, 26, 0),		OPS_MASK,		{ FD, FB},			"Paired Single Reciprocal Square Root Estimate"	},
	{	gekko_ps_rsqrte_dot,	"ps_rsqrte.",	OPSC(4, 26, 1),		OPS_MASK_DOT,	{ FD, FB},			"Paired Single Reciprocal Square Root Estimate"	},
	{	gekko_ps_msub,			"ps_msub",		OPSC(4, 28, 0),		OPS_MASK,		{ FD, FA, FC, FB},	"Paired Single Multiply-Subtract"	},
	{	gekko_ps_msub_dot,		"ps_msub.",		OPSC(4, 28, 1),		OPS_MASK_DOT,	{ FD, FA, FC, FB},	"Paired Single Multiply-Subtract"	},
	{	gekko_ps_madd,			"ps_madd",		OPSC(4, 29, 0),		OPS_MASK,		{ FD, FA, FC, FB},	"Paired Single Multiply-Add"	},
	{	gekko_ps_madd_dot,		"ps_madd.",		OPSC(4, 29, 1),		OPS_MASK_DOT,	{ FD, FA, FC, FB},	"Paired Single Multiply-Add"	},
	{	gekko_ps_nmsub,			"ps_nmsub",		OPSC(4, 30, 0),		OPS_MASK,		{ FD, FA, FC, FB},	"Paired Single Negative Multiply-Subtract"	},
	{	gekko_ps_nmsub_dot,		"ps_nmsub.",	OPSC(4, 30, 1),		OPS_MASK_DOT,	{ FD, FA, FC, FB},	"Paired Single Negative Multiply-Subtract"	},
	{	gekko_ps_nmadd,			"ps_nmadd",		OPSC(4, 31, 0),		OPS_MASK,		{ FD, FA, FC, FB},	"Paired Single Negative Multiply-Add"	},
	{	gekko_ps_nmadd_dot,		"ps_nmadd.",	OPSC(4, 31, 1),		OPS_MASK_DOT,	{ FD, FA, FC, FB},	"Paired Single Negative Multiply-Add"	},

	{	gekko_ps_neg,			"ps_neg",		OPLC(4, 40, 0),		OPL_MASK,		{ FD, FB },			"Paired Single Negate"	},
	{	gekko_ps_neg_dot,		"ps_neg.",		OPLC(4, 40, 1),		OPL_MASK_DOT,	{ FD, FB },			"Paired Single Negate"	},
	{	gekko_ps_mr,			"ps_mr",		OPLC(4, 72, 0),		OPL_MASK,		{ FD, FB },			"Paired Single Move Register"	},
	{	gekko_ps_mr_dot,		"ps_mr.",		OPLC(4, 72, 1),		OPL_MASK_DOT,	{ FD, FB },			"Paired Single Move Register"	},
	{	gekko_ps_nabs,			"ps_nabs",		OPLC(4, 136, 0),	OPL_MASK,		{ FD, FB },			"Paired Single Negative Absolute Value"	},
	{	gekko_ps_nabs_dot,		"ps_nabs.",		OPLC(4, 136, 1),	OPL_MASK_DOT,	{ FD, FB },			"Paired Single Negative Absolute Value"	},
	{	gekko_ps_abs,			"ps_abs",		OPLC(4, 264, 0),	OPL_MASK,		{ FD, FB },			"Paired Single Absolute Value"	},
	{	gekko_ps_abs_dot,		"ps_abs.",		OPLC(4, 264, 1),	OPL_MASK_DOT,	{ FD, FB },			"Paired Single Absolute Value"	},

	{	gekko_ps_sum0,			"ps_sum0",		OPSC(4, 10, 0),		OPS_MASK,		{ FD, FA, FC, FB },	"Paired Single vector SUM high"	},
	{	gekko_ps_sum0_dot,		"ps_sum0.",		OPSC(4, 10, 1),		OPS_MASK_DOT,	{ FD, FA, FC, FB },	"Paired Single vector SUM high"	},
	{	gekko_ps_sum1,			"ps_sum1",		OPSC(4, 11, 0),		OPS_MASK,		{ FD, FA, FC, FB },	"Paired Single vector SUM low"	},
	{	gekko_ps_sum1_dot,		"ps_sum1.",		OPSC(4, 11, 1),		OPS_MASK_DOT,	{ FD, FA, FC, FB },	"Paired Single vector SUM low"	},
	{	gekko_ps_muls0,			"ps_muls0",		OPSC(4, 12, 0),		OPS_MASK,		{ FD, FA, FC },		"Paired Single Multiply Scalar high"	},
	{	gekko_ps_muls0_dot,		"ps_muls0.",	OPSC(4, 12, 1),		OPS_MASK_DOT,	{ FD, FA, FC },		"Paired Single Multiply Scalar high"	},
	{	gekko_ps_muls1,			"ps_muls1",		OPSC(4, 13, 0),		OPS_MASK,		{ FD, FA, FC },		"Paired Single Multiply Scalar low"		},
	{	gekko_ps_muls1_dot,		"ps_muls1.",	OPSC(4, 13, 1),		OPS_MASK_DOT,	{ FD, FA, FC },		"Paired Single Multiply Scalar low"		},
	{	gekko_ps_madds0,		"ps_madds0",	OPSC(4, 14, 0),		OPS_MASK,		{ FD, FA, FC, FB },	"Paired Single Multiply-Add Scalar high"},
	{	gekko_ps_madds0_dot,	"ps_madds0.",	OPSC(4, 14, 1),		OPS_MASK_DOT,	{ FD, FA, FC, FB },	"Paired Single Multiply-Add Scalar high"},
	{	gekko_ps_madds1,		"ps_madds1",	OPSC(4, 15, 0),		OPS_MASK,		{ FD, FA, FC, FB },	"Paired Single Multiply-Add Scalar low"	},
	{	gekko_ps_madds1_dot,	"ps_madds1.",	OPSC(4, 15, 1),		OPS_MASK_DOT,	{ FD, FA, FC, FB },	"Paired Single Multiply-Add Scalar low"	},

	{	gekko_ps_cmpu0,			"ps_cmpu0",		OPL(4, 0),			OPL_MASK,		{ crfD, FA, FB },	"Paired Singles Compare Unordered High"	},
	{	gekko_ps_cmpo0,			"ps_cmpo0",		OPL(4, 32),			OPL_MASK,		{ crfD, FA, FB },	"Paired Singles Compare Ordered High"	},
	{	gekko_ps_cmpu1,			"ps_cmpu1",		OPL(4, 64),			OPL_MASK,		{ crfD, FA, FB },	"Paired Singles Compare Unordered Low"	},
	{	gekko_ps_cmpo1,			"ps_cmpo1",		OPL(4, 96),			OPL_MASK,		{ crfD, FA, FB },	"Paired Singles Compare Ordered Low"	},

	{	gekko_ps_merge00,		"ps_merge00",	OPLC(4, 528, 0),	OPL_MASK,		{ FD, FA, FB },		"Paired Single MERGE high"		},
	{	gekko_ps_merge00_dot,	"ps_merge00.",	OPLC(4, 528, 1),	OPL_MASK_DOT,	{ FD, FA, FB },		"Paired Single MERGE high"		},
	{	gekko_ps_merge01,		"ps_merge01",	OPLC(4, 560, 0),	OPL_MASK,		{ FD, FA, FB },		"Paired Single MERGE direct"	},
	{	gekko_ps_merge01_dot,	"ps_merge01.",	OPLC(4, 560, 1),	OPL_MASK_DOT,	{ FD, FA, FB },		"Paired Single MERGE direct"	},
	{	gekko_ps_merge10,		"ps_merge10",	OPLC(4, 592, 0),	OPL_MASK,		{ FD, FA, FB },		"Paired Single MERGE swapped"	},
	{	gekko_ps_merge10_dot,	"ps_merge10.",	OPLC(4, 592, 1),	OPL_MASK_DOT,	{ FD, FA, FB },		"Paired Single MERGE swapped"	},
	{	gekko_ps_merge11,		"ps_merge11",	OPLC(4, 624, 0),	OPL_MASK,		{ FD, FA, FB },		"Paired Single MERGE low"		},
	{	gekko_ps_merge11_dot,	"ps_merge11.",	OPLC(4, 624, 1),	OPL_MASK_DOT,	{ FD, FA, FB },		"Paired Single MERGE low"		},

	{	gekko_ps_dcbz_l,		"dcbz_l",		OPL(4, 1014),		OPL_MASK,		{ RA, RB },			"Data Cache Block Set to Zero Locked"	},

};


/***************************************************************************************************
*
*	FUNCTION		PluginAnalyse
*
*	DESCRIPTION		This is the main analysis function..
*
***************************************************************************************************/

int	PluginAnalyse(insn_t *_insn)
{
	insn_t &insn = *_insn;
	// Get the CodeBytes
	uint32 codeBytes = get_dword(insn.ea);

	// When we check
	uint32 opBytes = (codeBytes & OP_MASK);

	// All our opcodes have a primary op setting of 4 or 56, 57, 60, 61. 
	if ((opBytes == OP(4)) ||
		(opBytes == OP(56)) ||
		(opBytes == OP(57)) ||
		(opBytes == OP(60)) ||
		(opBytes == OP(61)))
	{
		int	opcodeArraySize = sizeof(g_gekkoPsOpcodes) / sizeof(gekko_ps_opcode);
		gekko_ps_opcode*	pCurrentOpcode = g_gekkoPsOpcodes;

		// Go through the entire opcode array looking for a match
		for (int opcodeLoop = 0; opcodeLoop < opcodeArraySize; opcodeLoop++)
		{
			// Is this a match?
			if ((codeBytes & pCurrentOpcode->mask) == pCurrentOpcode->opcode)
			{
				// Ok, so we've got a match.. let's sort out the operands..
				int operandLoop = 0;
				while ((pCurrentOpcode->operands[operandLoop] != 0) && (operandLoop < 6))
				{
					op_t* operandData = &insn.ops[operandLoop];

					gekko_ps_operand* pCurrentOperand = &g_gekkoPsOperands[pCurrentOpcode->operands[operandLoop]];

					int	rawBits = (codeBytes >> pCurrentOperand->shift) & ((1 << pCurrentOperand->bits) - 1);
					int	extendedBits = (rawBits << (32 - pCurrentOperand->bits)) >> (32 - pCurrentOperand->bits);

					switch (pCurrentOpcode->operands[operandLoop])
					{
						// These are the main Gekko registers
					case	FA:
					case	FB:
					case	FC:
					case	FD://FS
					//case	FS:
					{
						operandData->type = o_reg;
						operandData->reg = rawBits;
						operandData->specflag1 = 0x01;		// Mark the register as being an Gekko one.
						break;
					}

					// Gekko PS memory loads are always via a CPU register
					case	RA:
					case	RB:
					{
						operandData->type = o_reg;
						operandData->reg = rawBits;
						operandData->specflag1 = 0x00;
						break;
					}

					case	crfD:
					case	WB:
					case	IB:
					case	WC:
					case	IC:
					{
						operandData->type = o_imm;
						operandData->dtype = dt_byte;
						operandData->value = rawBits;
						break;
					}

					case	DRA:
					{
						unsigned short imm = (unsigned short)(codeBytes & 0x7FF);
						unsigned short sign = (unsigned short)(codeBytes & 0x800);
						short displacement = 0;

						if (sign == 0)
							displacement = imm;
						else
							displacement = -1 * imm;


						operandData->type = o_displ;
						operandData->phrase = rawBits;
						operandData->addr = displacement;

						break;
					}

					default:
						break;
					}

					// Next operand please..
					operandLoop++;
				}

				// Make a note of which opcode we are.. we need it to print our stuff out.
				insn.itype = pCurrentOpcode->insn;

				// The command is 4 bytes long.. 
				return 4;
			}

			// We obviously didn't find our opcode this time round.. go test the next one.
			pCurrentOpcode++;
		}
	}

	// We didn't do anything.. honest.	
	return 0;
}


/***************************************************************************************************
*
*	FUNCTION		PluginExtensionCallback
*
*	DESCRIPTION		This callback is responsible for distributing work associated with each
*					intercepted event that we deal with. In our case we deal with the following
*					event identifiers.
*
*					custom_ana		:	Analyses a command (in 'cmd') to see if it is an Gekko PS
*										instruction. If so, then it extracts information from the
*										opcode in order to determine which opcode it is, along with
*										data relating to any used operands.
*
*					custom_mnem		:	Generates the mnemonic for our Gekko PS instructions, by looking
*										into our array of opcode information structures.
*
*					custom_outop	:	Outputs operands for Gekko PS instructions. In our case, we
*										have an alternate register set (vr0 to vr31), so our operands
*										may be marked as being Gekko registers.
*
*					may_be_func		:	It's perfectly OK for an Gekko PS instruction to be the start
*										of a function, so I figured I should return 100 here. The
*										return value is a percentage probability..
*
*					is_sane_insn	:	All our Gekko PS instructions (well, the ones we've identified
*										inside custom_ana processing), are ok.
*
***************************************************************************************************/

static ssize_t idaapi PluginExtensionCallback(void * user_data, int notification_code, va_list va)
{
	switch (notification_code)
	{
		// Analyse a command to see if it's an Gekko PS instruction.
	case processor_t::ev_ana_insn:
	{
		insn_t *_insn = va_arg(va, insn_t *);

		int length = PluginAnalyse(_insn);
		if (length)
		{
			_insn->size = length;
			//return (length + 1);	// event processed
			return length;	// event processed
		}
		break;
	}

	// Obtain mnemonic for our Gekko PS instructions.
	case processor_t::ev_out_mnem:
	{
		outctx_t * _ctx = va_arg(va, outctx_t *);

		if (_ctx->insn.itype >= CUSTOM_INSN_ITYPE)
		{
			_ctx->out_custom_mnem(g_gekkoPsOpcodes[_ctx->insn.itype - gekko_ps_insn_start].name, 10, " ");
			return 1;
		}
		break;
	}

	// Display operands that differ from PPC ones.. like our Gekko PS registers.
	case processor_t::ev_out_operand:
	{
		outctx_t * _ctx = va_arg(va, outctx_t *);

		if (_ctx->insn.itype >= CUSTOM_INSN_ITYPE)
		{
			op_t* operand = va_arg(va, op_t*);

			if ((operand->type == o_reg) && (operand->specflag1 & 0x01))
			{
				char buf[MAXSTR] = { 0 };
				_snprintf_s(buf, MAXSTR, "fr%d", operand->reg);
				_ctx->out_register(buf);
				return 1;
			}
		}
		break;
	}

	// dunno why not work, seems an IDA bug???
	case processor_t::ev_get_autocmt:
	{
		qstring * buf = va_arg(va, qstring *);
		const ::insn_t * insn = va_arg(va, const ::insn_t *);
		if (insn->itype >= CUSTOM_INSN_ITYPE)
		{
			// Output auto comments
			//if (show_all_comments() && (get_cmt(NULL, insn->ea, true) == -1))
			{
				*buf = g_gekkoPsOpcodes[insn->itype - gekko_ps_insn_start].description;
			}
			return 1;
		}
		break;
	}

	// Can this be the start of a function? 
	case processor_t::ev_may_be_func:
	{
		insn_t *_insn = va_arg(va, insn_t *);
		if (_insn->itype >= CUSTOM_INSN_ITYPE)
		{
			return 100;
		}
		break;
	}

	// If we've identified the command as an Gekko PS instruction, it's good to go.
	case processor_t::ev_is_sane_insn:
	{
		insn_t *_insn = va_arg(va, insn_t *);
		if (_insn->itype >= CUSTOM_INSN_ITYPE)
		{
			return 1;
		}
	}
	default:
	{
		__noop;
	}
	}

	// We didn't process the event.. just let IDA handle it.
	return 0;
}


/***************************************************************************************************
*
*	FUNCTION		PluginStartup
*
*	DESCRIPTION		IDA will call this function only once. If this function returns PLUGIN_SKIP,
*					IDA will never load it again. If it returns PLUGIN_OK, IDA will unload the plugin
*					but remember that the plugin agreed to work with the database. The plugin will
*					be loaded again if the user invokes it by pressing the hotkey or selecting it
*					from the menu. After the second load, the plugin will stay in memory.
*
*	NOTES			In our case, we just hook into IDA'S callbacks if we need to be active
*					on plugin load.
*
***************************************************************************************************/

int	PluginStartup(void)
{
	if (ph.id != PLFM_PPC)
		return PLUGIN_SKIP;

	//inf.s_cmtflg |= SW_ALLCMT;
	// Debug stuff to identify auto-comment status
	/*
	if ( showAllComments() )
		msg( "All comments enabled\n" );
	else
		msg( "All comments disabled\n" );
	//*/
	// Create our node...
	g_GekkoNode.create(g_GekkoNodeName);

	// Retrieve any existing hook state that may be in the database.
	HookState	databaseHookState = (HookState)g_GekkoNode.altval(0);

	// altval() returns 0 (which maps to kDefault) when the value isn't there.. so handle it.
	if (databaseHookState != kDefault)
		g_HookState = databaseHookState;

	if (g_HookState == kEnabled)
	{
		hook_to_notification_point(HT_IDP, PluginExtensionCallback, NULL);
		msg("Nintendo GameCube Gekko CPU Extension " GEKKO_VERSION " is enabled\n");
		msg("This plug-in was created by HyperIris (fsstudio@263.net)\n");
		return PLUGIN_KEEP;
	}

	//msg( "Nintendo GameCube Gekko CPU Extension Plug-in by HyperIris (fsstudio@263.net)\n" );
	return PLUGIN_OK;
}

/***************************************************************************************************
*
*	FUNCTION		PluginShutdown
*
*	DESCRIPTION		IDA will call this function when the user asks to exit. This function is *not*
*					called in the case of emergency exits.
*
*   NOTES			All we can do here is to release from our callbacks..
*
***************************************************************************************************/

void	PluginShutdown(void)
{
	unhook_from_notification_point(HT_IDP, PluginExtensionCallback);
}


/***************************************************************************************************
*
*	FUNCTION		PluginMain
*
*	DESCRIPTION		Our plugin is all about hooking callbacks..
*
***************************************************************************************************/

bool PluginMain(size_t /*arg*/)
{
	if (g_HookState == kEnabled)
	{
		unhook_from_notification_point(HT_IDP, PluginExtensionCallback);
		g_HookState = kDisabled;
	}
	else
		if (g_HookState == kDisabled)
		{
			hook_to_notification_point(HT_IDP, PluginExtensionCallback, NULL);
			g_HookState = kEnabled;
		}

	g_GekkoNode.create(g_GekkoNodeName);
	g_GekkoNode.altset(0, g_HookState);

	static const char* pHookStateDescription[] =
	{
		"default",
		"enabled",
		"disabled",
	};

	info("AUTOHIDE NONE\n"
		"Nintendo GameCube Gekko CPU Extension " GEKKO_VERSION " is now %s", pHookStateDescription[g_HookState]);
	//msg("Nintendo GameCube Gekko CPU Extension ")

	return true;
}


/***************************************************************************************************
*
*	Strings required for IDA Pro's PLUGIN descriptor block
*
***************************************************************************************************/

char	g_pluginName[] = "Nintendo GameCube Gekko CPU Extension " GEKKO_VERSION;
char	g_pluginHelp[] = "This plugin enables recognition of Gekko CPU Extension instructions\n"
"when using IDA Pro's PowerPC processor module\n";


/***************************************************************************************************
*
*	This 'PLUGIN' data block is how IDA Pro interfaces with this plugin.
*
***************************************************************************************************/
plugin_t PLUGIN =
{
	IDP_INTERFACE_VERSION,
	0,						// plugin flags	
	PluginStartup,			// initialize
	PluginShutdown,			// terminate. this pointer may be NULL.
	PluginMain,				// invoke plugin
	g_pluginName,			// long comment about the plugin. It could appear in the status line or as a hint
	g_pluginHelp,			// multiline help about the plugin
	g_pluginName,			// the preferred short name of the plugin
	""						// the preferred hotkey to run the plugin
};
