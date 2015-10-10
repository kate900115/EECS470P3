/////////////////////////////////////////////////////////////////////////
//                                                                     //
//   Modulename :  id_stage.v                                          //
//                                                                     //
//  Description :  instruction decode (ID) stage of the pipeline;      // 
//                 decode the instruction fetch register operands, and // 
//                 compute immediate operand (if applicable)           // 
//                                                                     //
/////////////////////////////////////////////////////////////////////////


`timescale 1ns/100ps


  // Decode an instruction: given instruction bits IR produce the
  // appropriate datapath control signals.
  //
  // This is a *combinational* module (basically a PLA).
  //
module decoder(

    input [31:0] inst,
    input [31:0] id_ex_inst, //kate
    input [31:0] ex_mem_inst, //kate
    input valid_inst_in,  // ignore inst when low, outputs will
                          // reflect noop (except valid_inst)
    input ex_mem_valid_inst,
    input id_ex_valid_inst,


    output ALU_OPA_SELECT opa_select,
    output ALU_OPB_SELECT opb_select,
    output DEST_REG_SEL   dest_reg, // mux selects
    output ALU_FUNC       alu_func,
    output logic rd_mem, wr_mem, cond_branch, uncond_branch,
    output logic halt,      // non-zero on a halt
    output logic illegal,    // non-zero on an illegal instruction
    output logic valid_inst  // for counting valid instructions executed
                            // and for making the fetch stage die on halts/
                            // keeping track of when to allow the next
                            // instruction out of fetch
                            // 0 for HALT and illegal instructions (die on halt)

  );

  assign valid_inst = valid_inst_in & ~illegal;
  
  always_comb begin
    // default control values:
    // - valid instructions must override these defaults as necessary.
    //   opa_select, opb_select, and alu_func should be set explicitly.
    // - invalid instructions should clear valid_inst.
    // - These defaults are equivalent to a noop
    // * see sys_defs.vh for the constants used here
    opa_select = ALU_OPA_IS_REGA;
    opb_select = ALU_OPB_IS_REGB;
    alu_func = ALU_ADDQ;
    dest_reg = DEST_NONE;
    rd_mem = `FALSE;
    wr_mem = `FALSE;
    cond_branch = `FALSE;
    uncond_branch = `FALSE;
    halt = `FALSE;
    illegal = `FALSE;
    if(valid_inst_in) begin
      case ({inst[31:29], 3'b0})
        6'h0:
          case (inst[31:26])
            `PAL_INST:
              if (inst[25:0] == 26'h0555)
                halt = `TRUE;
              else
                illegal = `TRUE;
            default: illegal = `TRUE;
          endcase // case(inst[31:26])

        6'h10:
        begin //kate
          opa_select = 

	  (((inst[25:21]==id_ex_inst[4:0])&&(id_ex_inst[31:29]==3'b010)&&(id_ex_valid_inst))||
          ((id_ex_inst[31:26]==`LDA_INST)&&(inst[25:21]==id_ex_inst[25:21])&&(id_ex_valid_inst)))?ALU_OPA_IS_FORWARDING_EX:  	  //if id_ex stage instruction is R-type, and id_ex_IR[RC]=inst[RA]
          (((ex_mem_inst[31:26]==`LDQ_INST)&&(ex_mem_inst[25:21]==inst[25:21])&&(ex_mem_valid_inst))||			  //if ex_mem stage instruction is LOAD, and ex_mem_IR[RA]=inst[RA]
          ((ex_mem_inst[31:29]==3'b010)&&(inst[25:21]==ex_mem_inst[4:0])&&(ex_mem_valid_inst))||
          ((ex_mem_inst[31:26]==`LDA_INST)&&(inst[25:21]==ex_mem_inst[25:21])&&(ex_mem_valid_inst)))?ALU_OPA_IS_FORWARDING_MEM:	  //if ex_mem stage instruction is R-type, and ex_mem_IR[RC]=inst[RA]
          ALU_OPA_IS_REGA;

          opb_select = 
           
          inst[12] ? ALU_OPB_IS_ALU_IMM : 
          (((inst[20:16]==id_ex_inst[4:0])&&(id_ex_inst[31:29]==3'b010)&&(id_ex_valid_inst))||
          ((id_ex_inst[31:26]==`LDA_INST)&&(inst[20:16]==id_ex_inst[25:21])&&(id_ex_valid_inst)))?ALU_OPB_IS_FORWARDING_EX: 	  //if id_ex stage instruction is R-type, and id_ex_IR[RC]=inst[RB]
          (((ex_mem_inst[31:26]==`LDQ_INST)&&(ex_mem_inst[25:21]==inst[20:16])&&(ex_mem_valid_inst))||			  //if ex_mem stage instruction is LOAD, and ex_mem_IR[RC]=inst[RB]
          ((ex_mem_inst[31:29]==3'b010)&&(inst[20:16]==ex_mem_inst[4:0])&&(ex_mem_valid_inst))||
          ((ex_mem_inst[31:26]==`LDA_INST)&&(inst[20:16]==ex_mem_inst[25:21])&&(ex_mem_valid_inst)))? 				  //if ex_mem stage instruction is R-type, and ex_mem_IR[RC]=inst[RB]
	  ALU_OPB_IS_FORWARDING_MEM:ALU_OPB_IS_REGB;
          
          dest_reg = DEST_IS_REGC;

          case (inst[31:26])
            `INTA_GRP:
              case (inst[11:5])
                `CMPULT_INST:  alu_func = ALU_CMPULT;
                `ADDQ_INST:    alu_func = ALU_ADDQ;
                `SUBQ_INST:    alu_func = ALU_SUBQ;
                `CMPEQ_INST:   alu_func = ALU_CMPEQ;
                `CMPULE_INST:  alu_func = ALU_CMPULE;
                `CMPLT_INST:   alu_func = ALU_CMPLT;
                `CMPLE_INST:   alu_func = ALU_CMPLE;
                default:       illegal = `TRUE;
              endcase // case(inst[11:5])
            `INTL_GRP:
              case (inst[11:5])
                `AND_INST:    alu_func = ALU_AND;
                `BIC_INST:    alu_func = ALU_BIC;
                `BIS_INST:    alu_func = ALU_BIS;
                `ORNOT_INST:  alu_func = ALU_ORNOT;
                `XOR_INST:    alu_func = ALU_XOR;
                `EQV_INST:    alu_func = ALU_EQV;
                default:      illegal = `TRUE;
              endcase // case(inst[11:5])
            `INTS_GRP:
              case (inst[11:5])
                `SRL_INST:  alu_func = ALU_SRL;
                `SLL_INST:  alu_func = ALU_SLL;
                `SRA_INST:  alu_func = ALU_SRA;
                default:    illegal = `TRUE;
              endcase // case(inst[11:5])
            `INTM_GRP:
              case (inst[11:5])
                `MULQ_INST:       alu_func = ALU_MULQ;
                default:          illegal = `TRUE;
              endcase // case(inst[11:5])
            `ITFP_GRP:       illegal = `TRUE;       // unimplemented
            `FLTV_GRP:       illegal = `TRUE;       // unimplemented
            `FLTI_GRP:       illegal = `TRUE;       // unimplemented
            `FLTL_GRP:       illegal = `TRUE;       // unimplemented
          endcase // case(inst[31:26])
        end

        6'h18:
          case (inst[31:26])
            `MISC_GRP:       illegal = `TRUE; // unimplemented
            `JSR_GRP:
            begin
              // JMP, JSR, RET, and JSR_CO have identical semantics
              opa_select = ALU_OPA_IS_NOT3;
              opb_select = (((inst[20:16]==id_ex_inst[4:0])&&(id_ex_inst[31:29]==3'b010)&&(id_ex_valid_inst))||
                           ((id_ex_inst[31:26]==`LDA_INST)&&(inst[20:16]==id_ex_inst[25:21])&&(id_ex_valid_inst))||
                           ((id_ex_inst[31:26]==`LDQ_INST)&&(inst[20:16]==id_ex_inst[25:21])&&(id_ex_valid_inst)))? ALU_OPB_IS_FORWARDING_EX : 	//kate  //if id_ex stage instruction is R-type, and id_ex_IR[RC]=inst[RB]
                           (((ex_mem_inst[31:26]==`LDQ_INST)&&(ex_mem_inst[25:21]==inst[20:16])&&(ex_mem_valid_inst))||			        //if ex_mem stage instruction is LOAD, and ex_mem_IR[RA]=inst[RB]
                           ((ex_mem_inst[31:29]==3'b010)&&(inst[20:16]==ex_mem_inst[4:0])&&(ex_mem_valid_inst))||                               //if ex_mem stage instruction is R-type, and ex_mem_IR[RC]=inst[RB]
                           ((ex_mem_inst[31:26]==`LDA_INST)&&(inst[20:16]==ex_mem_inst[25:21])&&(ex_mem_valid_inst)))? 				
                           ALU_OPB_IS_FORWARDING_MEM:ALU_OPB_IS_REGB;
              alu_func = ALU_AND; // clear low 2 bits (word-align)
              dest_reg = DEST_IS_REGA;
              uncond_branch = `TRUE;
            end
            `FTPI_GRP:       illegal = `TRUE;       // unimplemented
          endcase // case(inst[31:26])
         
        6'h08, 6'h20, 6'h28:
        begin
          opa_select = ALU_OPA_IS_MEM_DISP;
          opb_select = (((inst[20:16]==id_ex_inst[4:0])&&(id_ex_inst[31:29]==3'b010)&&(id_ex_valid_inst))||
                       ((id_ex_inst[31:26]==`LDA_INST)&&(inst[20:16]==id_ex_inst[25:21])&&(id_ex_valid_inst)))?ALU_OPB_IS_FORWARDING_EX : 	//if id_ex stage instruction is R-type, and id_ex_IR[RC]=inst[RB]
                       (((ex_mem_inst[31:26]==`LDQ_INST)&&(ex_mem_inst[25:21]==inst[20:16])&&(ex_mem_valid_inst))||			        //if ex_mem stage instruction is LOAD, and ex_mem_IR[RA]=inst[RB]
                       ((ex_mem_inst[31:29]==3'b010)&&(inst[20:16]==ex_mem_inst[4:0])&&(ex_mem_valid_inst))||                                   //if ex_mem stage instruction is R-type, and ex_mem_IR[RC]=inst[RB]	
                       ((ex_mem_inst[31:26]==`LDA_INST)&&(inst[20:16]==ex_mem_inst[25:21])&&(ex_mem_valid_inst)))? 				//if id_ex stage instruction is LOAD, and id_ex_IR[RC]=inst[RB]	
                       /*((id_ex_inst[31:26]==`LDQ_INST)&&(inst[20:16]==id_ex_inst[25:21]))&&(id_ex_valid_inst))*/ 						
		       ALU_OPB_IS_FORWARDING_MEM:ALU_OPB_IS_REGB;
          alu_func = ALU_ADDQ;
          dest_reg = DEST_IS_REGA;
          case (inst[31:26])
            `LDA_INST:  /* defaults are OK */;
            `LDQ_INST:
              begin
                rd_mem = `TRUE;
                dest_reg = DEST_IS_REGA;
              end // case: `LDQ_INST
            `STQ_INST:
              begin
                wr_mem = `TRUE;
                dest_reg = DEST_NONE;
              end // case: `STQ_INST
            default:       illegal = `TRUE;
          endcase // case(inst[31:26])
        end
      
        6'h30, 6'h38:
        begin
          opa_select = ALU_OPA_IS_NPC;
          opb_select = ALU_OPB_IS_BR_DISP;
          alu_func = ALU_ADDQ;
          case (inst[31:26])
            `FBEQ_INST, `FBLT_INST, `FBLE_INST,
            `FBNE_INST, `FBGE_INST, `FBGT_INST:
            begin
              // FP conditionals not implemented
              illegal = `TRUE;
            end

            `BR_INST, `BSR_INST:
            begin
              dest_reg = DEST_IS_REGA;
              uncond_branch = `TRUE;
            end

            default:
              cond_branch = `TRUE; // all others are conditional
          endcase // case(inst[31:26])
        end
      endcase // case(inst[31:29] << 3)
    end // if(~valid_inst_in)
  end // always
     
endmodule // decoder


module id_stage(
             
        input         clock,              // system clock
        input         reset,              // system reset
        input  [31:0] if_id_IR,            // incoming instruction
	input  [31:0] id_ex_IR,            //kate 
	input  [31:0] ex_mem_IR, 	   //kate
        input         wb_reg_wr_en_out,    // Reg write enable from WB Stage
        input   [4:0] wb_reg_wr_idx_out,  // Reg write index from WB Stage
        input  [63:0] wb_reg_wr_data_out,  // Reg write data from WB Stage
        input         if_id_valid_inst,
        input         ex_mem_valid_inst,
        input         id_ex_valid_inst,
        input  [63:0] ex_alu_result_out, //kate
        input  [63:0] mem_result_out,//kate
        input         ex_mem_take_branch,//kate


        output logic [63:0] id_ra_value_out,    // reg A value
        output logic [63:0] id_rb_value_out,    // reg B value
        
        output ALU_OPA_SELECT id_opa_select_out,    // ALU opa mux select (ALU_OPA_xxx *)
        output ALU_OPB_SELECT id_opb_select_out,    // ALU opb mux select (ALU_OPB_xxx *)
        
        output logic [4:0] id_dest_reg_idx_out,  // destination (writeback) register index
        // (ZERO_REG if no writeback)
        output ALU_FUNC    id_alu_func_out,        // ALU function select (ALU_xxx *)
        output logic       id_rd_mem_out,          // does inst read memory?
        output logic       id_wr_mem_out,          // does inst write memory?
        output logic       id_cond_branch_out,     // is inst a conditional branch?
        output logic       id_uncond_branch_out,   // is inst an unconditional branch 
        // or jump?
        output logic       id_halt_out,
        output logic       id_illegal_out,
        output logic       id_valid_inst_out,  // is inst a valid instruction to be 
                                               // counted for CPI calculations?
	output logic	   ex_stall,
	output logic	   structure_hazard_stall
              );
   
  DEST_REG_SEL dest_reg_select;
  logic [63:0] id_ra_value_out_rf;

  // instruction fields read from IF/ID pipeline register
  wire    [4:0] ra_idx = if_id_IR[25:21];    // inst operand A register index
  wire    [4:0] rb_idx = if_id_IR[20:16];    // inst operand B register index
  wire    [4:0] rc_idx = if_id_IR[4:0];      // inst operand C register index

  // Instantiate the register file used by this pipeline
  regfile regf_0 (
    .rda_idx(ra_idx),
    .rda_out(id_ra_value_out_rf),//kate 

    .rdb_idx(rb_idx),
    .rdb_out(id_rb_value_out),

    .wr_clk(clock),
    .wr_en(wb_reg_wr_en_out),
    .wr_idx(wb_reg_wr_idx_out),
    .wr_data(wb_reg_wr_data_out)
  );

  // instantiate the instruction decoder
  decoder decoder_0 (
    // Input
    .inst(if_id_IR),
    .id_ex_inst( id_ex_IR ),   //KATE
    .ex_mem_inst(ex_mem_IR),
    .valid_inst_in(if_id_valid_inst),
    .id_ex_valid_inst(id_ex_valid_inst),
    .ex_mem_valid_inst(ex_mem_valid_inst),

    // Outputs
    .opa_select(id_opa_select_out),
    .opb_select(id_opb_select_out),
    .alu_func(id_alu_func_out),
    .dest_reg(dest_reg_select),
    .rd_mem(id_rd_mem_out),
    .wr_mem(id_wr_mem_out),
    .cond_branch(id_cond_branch_out),
    .uncond_branch(id_uncond_branch_out),
    .halt(id_halt_out),
    .illegal(id_illegal_out),
    .valid_inst(id_valid_inst_out)
  );

  // mux to generate dest_reg_idx based on
  // the dest_reg_select output from decoder
  always_comb begin
    case (dest_reg_select)
      DEST_IS_REGC: id_dest_reg_idx_out = rc_idx;
      DEST_IS_REGA: id_dest_reg_idx_out = ra_idx;
      DEST_NONE:    id_dest_reg_idx_out = `ZERO_REG;
      default:      id_dest_reg_idx_out = `ZERO_REG; 
    endcase
  end
 
  always_comb begin
    if ((id_ex_IR[31:26] == `LDQ_INST)&& id_ex_valid_inst && if_id_valid_inst) 
    begin
      if (((if_id_IR[31:29]==3'b010)&&(if_id_IR[12]==0)&&((if_id_IR[25:21]==id_ex_IR[25:21])||(if_id_IR[20:16]==id_ex_IR[25:21])))||
	 ((if_id_IR[31:29]==3'b010)&&(if_id_IR[12]==1)&&(if_id_IR[25:21]==id_ex_IR[25:21]))||
         ((if_id_IR[31:26]==`JSR_GRP)&&(if_id_IR[20:16]==id_ex_IR[25:21]))||
         (((if_id_IR[31:26]== `LDQ_INST)||(if_id_IR[31:26]== `STQ_INST))&&(if_id_IR[20:16]==id_ex_IR[25:21])||(if_id_IR[25:21]==id_ex_IR[25:21])))
      begin
      	ex_stall=`TRUE;
      end 
      else
      begin
	ex_stall=`FALSE;
      end
    end
    else 
    begin
	ex_stall=`FALSE;
    end
  end

 always_comb
  begin
    if(((id_ex_IR[31:26] == `LDQ_INST)||(id_ex_IR[31:26] == `STQ_INST))&&(id_ex_valid_inst))    
    //if the MEM stage is load or store, there will be a structure hazard.
    begin
      if(ex_mem_take_branch==0)
      begin  
        structure_hazard_stall=1;
      end
      else
      begin
        structure_hazard_stall=0;
      end
    end
    else
      structure_hazard_stall=0;
  end 

  always_comb
  begin
    id_ra_value_out = id_ra_value_out_rf;
    
    
    if ((if_id_IR[31:26]==`STQ_INST)&&(ex_mem_IR[31:26]==`LDQ_INST)&&(if_id_valid_inst)&&(ex_mem_valid_inst))  
    begin
      if (if_id_IR[25:21]==ex_mem_IR[25:21])
      begin
        id_ra_value_out = mem_result_out;
      end
    end
    else if ((if_id_IR[31:26]==`STQ_INST)&&(ex_mem_IR[31:26]==`LDA_INST)&&(if_id_valid_inst)&&(ex_mem_valid_inst))  
    begin
      if (if_id_IR[25:21]==ex_mem_IR[25:21])
      begin
        id_ra_value_out = mem_result_out;
      end
    end
    else if ((if_id_IR[31:26]==`STQ_INST)&&(ex_mem_IR[31:29]==3'b010)&&(if_id_valid_inst)&&(ex_mem_valid_inst))
    begin
      if (if_id_IR[25:21]==ex_mem_IR[4:0])
      begin
        id_ra_value_out = mem_result_out;
      end
    end

    if ((if_id_IR[31:26]==`STQ_INST)&&(id_ex_IR[31:26]==`LDA_INST)&&(if_id_valid_inst)&&(id_ex_valid_inst))  
    //if there is a store in ID stage, and it depends on the result of EX stage, we need to forward it to the pipeline register.
    begin
      if(if_id_IR[25:21]==id_ex_IR[25:21])
      begin
        id_ra_value_out = ex_alu_result_out;
      end
    end
    else if ((if_id_IR[31:26]==`STQ_INST)&&(id_ex_IR[31:29]==3'b010)&&(if_id_valid_inst)&&(id_ex_valid_inst))
    begin
      if (if_id_IR[25:21]==id_ex_IR[4:0])
      begin
        id_ra_value_out = ex_alu_result_out;
      end
    end


    if (((if_id_IR[31:26]==`BEQ_INST)||(if_id_IR[31:26]==`BNE_INST)||(if_id_IR[31:26]==`BLE_INST)||(if_id_IR[31:26]==`BSR_INST)||
              (if_id_IR[31:26]==`BLT_INST)||(if_id_IR[31:26]==`BGT_INST)||(if_id_IR[31:26]==`BGE_INST)||(if_id_IR[31:26]==`BR_INST)||
              (if_id_IR[31:26]==`BLBC_INST)||(if_id_IR[31:26]==`BLBS_INST))&&(ex_mem_IR[31:26]==`LDA_INST)&&(if_id_valid_inst)&&(ex_mem_valid_inst))
    begin
      if(if_id_IR[25:21]==ex_mem_IR[25:21])
      begin
        id_ra_value_out = mem_result_out;
      end
    end
     
    else if (((if_id_IR[31:26]==`BEQ_INST)||(if_id_IR[31:26]==`BNE_INST)||(if_id_IR[31:26]==`BLE_INST)||(if_id_IR[31:26]==`BSR_INST)||
              (if_id_IR[31:26]==`BLT_INST)||(if_id_IR[31:26]==`BGT_INST)||(if_id_IR[31:26]==`BGE_INST)||(if_id_IR[31:26]==`BR_INST)||
              (if_id_IR[31:26]==`BLBC_INST)||(if_id_IR[31:26]==`BLBS_INST))&&(ex_mem_IR[31:26]==`LDQ_INST)&&(if_id_valid_inst)&&(ex_mem_valid_inst))
    begin
      if(if_id_IR[25:21]==ex_mem_IR[25:21])
      begin
        id_ra_value_out = mem_result_out;
      end
    end
    else if (((if_id_IR[31:26]==`BEQ_INST)||(if_id_IR[31:26]==`BNE_INST)||(if_id_IR[31:26]==`BLE_INST)||(if_id_IR[31:26]==`BSR_INST)||
              (if_id_IR[31:26]==`BLT_INST)||(if_id_IR[31:26]==`BGT_INST)||(if_id_IR[31:26]==`BGE_INST)||(if_id_IR[31:26]==`BR_INST)||
              (if_id_IR[31:26]==`BLBC_INST)||(if_id_IR[31:26]==`BLBS_INST))&&(ex_mem_IR[31:29]==3'b010)&&(if_id_valid_inst)&&(ex_mem_valid_inst))
    begin
      if (if_id_IR[25:21]==ex_mem_IR[4:0])
      begin
        id_ra_value_out = mem_result_out;
      end
    end

    if (((if_id_IR[31:26]==`BEQ_INST)||(if_id_IR[31:26]==`BNE_INST)||(if_id_IR[31:26]==`BLE_INST)||(if_id_IR[31:26]==`BSR_INST)||
              (if_id_IR[31:26]==`BLT_INST)||(if_id_IR[31:26]==`BGT_INST)||(if_id_IR[31:26]==`BGE_INST)||(if_id_IR[31:26]==`BR_INST)||
              (if_id_IR[31:26]==`BLBC_INST)||(if_id_IR[31:26]==`BLBS_INST))&&(id_ex_IR[31:26]==`LDA_INST)&&(if_id_valid_inst)&&(id_ex_valid_inst))
    begin
      if(if_id_IR[25:21]==id_ex_IR[25:21])
      begin
        id_ra_value_out = ex_alu_result_out;
      end
    end
    else if (((if_id_IR[31:26]==`BEQ_INST)||(if_id_IR[31:26]==`BNE_INST)||(if_id_IR[31:26]==`BLE_INST)||(if_id_IR[31:26]==`BSR_INST)||
              (if_id_IR[31:26]==`BLT_INST)||(if_id_IR[31:26]==`BGT_INST)||(if_id_IR[31:26]==`BGE_INST)||(if_id_IR[31:26]==`BR_INST)||
              (if_id_IR[31:26]==`BLBC_INST)||(if_id_IR[31:26]==`BLBS_INST))&&(id_ex_IR[31:29]==3'b010)&&(if_id_valid_inst)&&(id_ex_valid_inst))
    begin
      if (if_id_IR[25:21]==id_ex_IR[4:0])
      begin
        id_ra_value_out = ex_alu_result_out;
      end
    end
 

    else if (((if_id_IR[31:26]==`BEQ_INST)||(if_id_IR[31:26]==`BNE_INST)||(if_id_IR[31:26]==`BLE_INST)||(if_id_IR[31:26]==`BSR_INST)||
              (if_id_IR[31:26]==`BLT_INST)||(if_id_IR[31:26]==`BGT_INST)||(if_id_IR[31:26]==`BGE_INST)||(if_id_IR[31:26]==`BR_INST)||
              (if_id_IR[31:26]==`BLBC_INST)||(if_id_IR[31:26]==`BLBS_INST))&&(id_ex_IR[31:26]==`LDQ_INST)&&(if_id_valid_inst)&&(id_ex_valid_inst))
    begin
      if(if_id_IR[25:21]==id_ex_IR[25:21])
      begin
        id_ra_value_out = mem_result_out;
      end
    end


    
  end
   
endmodule // module id_stage
