datatype variable = string_id of string;
type boolean_constant = bool;
type integer_constant = int;

datatype arithmetic_op = Div|Plus|Minus|Times 
datatype boolean_op = And|Or; 	
datatype relational_op = Lt|Le|Eq|Ne|Ge|Gt;

datatype Interger_Expression = IE_int_const of integer_constant | 
                               IE_var of variable |
			       IE of (Interger_Expression * Interger_Expression * arithmetic_op);
								
datatype Boolean_expression = BE_bool_cont of boolean_constant | 
                              BE_var of variable | 
			      Bool_Rel_Op of (Interger_Expression * Interger_Expression * relational_op )|
                              Bool_Bool_Op of (Boolean_expression * Boolean_expression * boolean_op);
							  
datatype Expression = Exp1 of Interger_Expression | 
                      Exp2 of Boolean_expression;
					  
					  

datatype Instruction = Skip |
                       Inst_assign of (variable * Expression) | 
		       Inst_Compd_list of Instruction list | 
		       Inst_cond of ( Boolean_expression * Instruction * Instruction)|
		       Inst_loop of ( Boolean_expression * Instruction);


datatype Type = Boolean_type|Integer_type;

type Declaration = variable * Type;
type Declaration_list = Declaration list;
type Program= Declaration_list * Instruction;

(*num Integer; *)
val num = string_id("num");
val num_declaration = (num, Integer_type):Declaration;

(*sum Integer;*)
val sum = string_id("sum");
val sum_declaration = (sum, Integer_type):Declaration;

(*i Integer;*)
val i = string_id("i");
val i_declaration = (i, Integer_type):Declaration;

(*num = 12;*)
val num_initialize = Inst_assign(num,Exp1(IE_int_const(12)));
 
(*sum = 0;*)
val sum_initialize = Inst_assign(sum,Exp1(IE_int_const(0)));
(*i = 0;*)
val i_initialize = Inst_assign(i,Exp1(IE_int_const(2)));

(*sum + i*)
val expression1 = IE(IE_var(sum), IE_var(i), Plus);
(*sum = sum + i*)
val sum_exp1 = Inst_assign(sum, Exp1(expression1));

(*num / i*)
val expression2 = IE(IE_var(num), IE_var(i), Div);
(*num = num / i*)
val num_exp1 = Inst_assign(num, Exp1(expression2));

(*i+1*)
val expression3 = IE(IE_var(i), IE_int_const(1), Plus);
(*i=i+1*)
val i_exp1 = Inst_assign(i, Exp1(expression3));

(*sum+num*)
val expression4 = IE(IE_var(sum), IE_var(num), Plus);
(*sum=sum+num*)
val sum_exp2 = Inst_assign(sum, Exp1(expression4));

(*num GT 0*)
val condition1 =  Bool_Rel_Op(IE_var(num),IE_int_const(0), Gt);

(*(i Times i Le num) *)

(*i Times i*)
val exp3 = IE(IE_var(i), IE_var(i), Times)

(*(i Times i Le num) *)
val condition2 = Bool_Rel_Op(exp3, IE_var(num), Le);

(*(num Minus i Times (num Div i)*)

(*num Minus i*)
val exp1 = IE(IE_var(num), IE_var(i), Minus);

(*num Div i*)
val exp2 = IE(IE_var(num), IE_var(i), Div);

(*(num Minus i Times (num Div i)*)
val expression5 = IE(exp1, exp2, Times);

(*(num Minus i Times (num Div i) Eq 0*)
val condition3 =  Bool_Rel_Op(expression5,IE_int_const(0), Eq);

(*Discussed in meeting*)
(*inner while*)
(*WHILE ((num Minus i Times (num Div i)) Eq 0)*)
(*sum = sum Plus i; 
	num = num Div i;*)
val inner_while_instr_list = Inst_Compd_list([sum_exp1,num_exp1]);
val while_inner = Inst_loop(condition3,inner_while_instr_list);

(*outer while*)
(*WHILE (i Times i Le num)*)
val outer_while_instr_list = Inst_Compd_list([while_inner,i_exp1]);
val while_outer = Inst_loop(condition2,outer_while_instr_list);

(*outer if-then-else*)
val outer_if_instr_list = Inst_Compd_list([while_outer,sum_exp2]);

val if_else_inst = Inst_cond(condition1,outer_if_instr_list,sum_initialize);

(* whole program block *)
(* combine declarations, initialization and if-else *)
val minSumOfFactors = Inst_Compd_list([num_initialize, sum_initialize, i_initialize, if_else_inst]);











