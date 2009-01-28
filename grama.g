'root' program(->P)
       generate_three_address_code(P -> TAC)
      print(TAC)

'type' IDENT

'type' Program
  program(DeclarationList,InstructionList)

'type' DeclarationList
  declaration_list(Declaration,DeclarationList)
  nil

'type' Declaration
  const_list(ConstList)
  list(List)

'type' ConstList
  nil 
  const_list(ConstObject, ConstList)

'type' ConstObject
  is(IDENT, Expr)

'type' List
  nil
  list(Did, List)

'type' Did
  did1(Tab)
  did2(IDENT)

'type' Tab
  tab1(IDENT,Expr)
  tab2(IDENT,Expr,Expr)
  tab3(IDENT,Expr,Expr,Expr)

'type' InstructionList
  instruction_list(Instruction,InstructionList)
  nil

'type' Instruction
  if(Expr,Instruction)
  if_else(Expr,Instruction,Instruction)
  while(Expr, Instruction)
  for(Expr,Expr,Expr, Instruction)
  do(Instruction,Expr)
  nil
  goto(IDENT)
  label(IDENT)
  return
  break
  continue
  expr(Expr)
  block(DeclarationList,InstructionList)
  
'type' Expr
  nil
  coma(Expr, Expr)
  is(IDENT, Expr)
  or(Expr, Expr)
  and(Expr, Expr)
  same(Expr, Expr)
  not_same(Expr, Expr)
  less(Expr, Expr)
  more(Expr, Expr)
  same_less(Expr, Expr)
  same_more(Expr, Expr)
  plus(Expr, Expr)
  minus(Expr, Expr)
  mult(Expr, Expr)
  div(Expr, Expr)
  mod(Expr, Expr)
  negation(Expr)
  pp_ident(IDENT)
  mm_ident(IDENT)
  num(INT)
  ident(IDENT)
  tab(Tab)
  ident_mm(IDENT)
  ident_pp(IDENT)
  

'nonterm' program(->Program)
  'rule' program(->program(X,Y)): declaration_list(->X) instruction_list(->Y) 

'nonterm' declaration_list(->DeclarationList)
  'rule' declaration_list(->nil):
  'rule' declaration_list(->declaration_list(X,Y)): declaration(->X) declaration_list(->Y)

'nonterm' declaration(->Declaration)
  'rule' declaration(->const_list(X)): "const" "int" const_list(->X) ";"
  'rule' declaration(->list(X)): "int" list(->X) ";"

'nonterm' const_list(->ConstList)
  'rule' const_list(->const_list(X,nil)):const_object(->X)
  'rule' const_list(->const_list(X,Y)): const_object(->X) "," const_list(->Y)
   

'nonterm' const_object(->ConstObject)
  'rule' const_object(->is(X,Y)): Ident(->X) "=" expr2(->Y)

'nonterm' list(->List)
  'rule' list(->list(X, nil)): did(->X)
  'rule' list(->list(X,Y)): did(->X) "," list(->Y)

'nonterm' did(->Did)
  'rule' did(->did1(X)): tab(->X)
  'rule' did(->did2(X)): Ident(->X)
  'rule' did(->did2(X)): Ident(->X) "=" expr2(->Y)

'nonterm' tab(->Tab)
  'rule' tab(->tab1(X,Y)):  Ident(->X) "[" expr(->Y) "]"
  'rule' tab(->tab2(X,Y,Z)):  Ident(->X) "[" expr(->Y) "]" "[" expr(->Z) "]" 
  'rule' tab(->tab3(X,Y,Z,W)):  Ident(->X) "[" expr(->Y) "]" "[" expr(->Z) "]" "[" expr(->W) "]"

'nonterm' instruction_list(->InstructionList)
  'rule' instruction_list(->instruction_list(X,Y)): instruction(->X) instruction_list(->Y)
  'rule' instruction_list(->nil):

'nonterm' block(->Instruction)
  'rule' block(->block(X,Y)): "{" declaration_list(->X) instruction_list(->Y) "}" 

'nonterm' instruction(->Instruction)
  'rule' instruction(->X): open_statement(->X)
  'rule' instruction(->X): closed_statement(->X)

'nonterm' open_statement(->Instruction)
  'rule' open_statement(->if(X,Y)): "if" "(" expr(->X) ")" instruction(->Y) 
  'rule' open_statement(->if_else(X,Y,Z)): "if" "(" expr(->X) ")" closed_statement(->Y) "else" open_statement(->Z) 
  'rule' open_statement(->while(X,Y)): "while" "(" expr(->X) ")" open_statement(->Y)   
  'rule' open_statement(->for(X,Y,Z,U)): "for" "(" expr_for(->X) ";" expr_for(->Y) ";" expr_for(->Z) ")" open_statement(->U)

'nonterm' closed_statement(->Instruction)
  'rule' closed_statement(->X): simple_statement(->X)
  'rule' closed_statement(->if_else(X,Y,Z)): "if" "(" expr(->X) ")" closed_statement(->Y) "else" closed_statement(->Z) 
  'rule' closed_statement(->do(Y,Z)): "do" instruction(->Y) "while" "(" expr(->Z) ")"
  'rule' closed_statement(->while(X,Y)): "while" "(" expr(->X) ")" closed_statement(->Y)
  'rule' closed_statement(->for(X,Y,Z,U)): "for" "(" expr_for(->X) ";" expr_for(->Y) ";" expr_for(->Z) ")" closed_statement(->U)
   
'nonterm' simple_statement(->Instruction)
  'rule' simple_statement(->break): "break" ";"
  'rule' simple_statement(->continue): "continue" ";"
  'rule' simple_statement(->goto(X)): "goto" Ident(->X) ";"
  'rule' simple_statement(->label(X)): Ident(->X) ":"
  'rule' simple_statement(->return): "return" ";"
  'rule' simple_statement(->expr(X)): expr(->X) ";"
  'rule' simple_statement(->nil): ";"
  'rule' simple_statement(->X): block(->X)


'nonterm' expr_for(->Expr)
  'rule' expr_for(->nil): 
  'rule' expr_for(->X): expr(->X)

'nonterm' expr(->Expr)
  'rule' expr(->X): expr2(->X)
  'rule' expr(->coma(X,Y)): expr(->X) "," expr2(->Y)      

'nonterm' expr2(->Expr)
  'rule' expr2(->X): expr3(->X)
  'rule' expr2(->is(X,Y)): Ident(->X) "=" expr2(->Y)

'nonterm' expr3(->Expr)
  'rule' expr3(->X): expr4(->X)
  'rule' expr3(->or(X,Y)): expr3(->X) "||" expr4(->Y)

'nonterm' expr4(->Expr)
  'rule' expr4(->X): expr5(->X)
  'rule' expr4(->and(X,Y)): expr4(->X) "&&" expr5(->Y)

'nonterm' expr5(->Expr)
  'rule' expr5(->X): expr6(->X)
  'rule' expr5(->same(X,Y)): expr5(->X) "==" expr6(->Y)
  'rule' expr5(->not_same(X,Y)): expr5(->X) "!=" expr6(->Y)

'nonterm' expr6(->Expr)
  'rule' expr6(->X): expr7(->X)
  'rule' expr6(->less(X,Y)): expr6(->X) "<" expr7(->Y)
  'rule' expr6(->more(X,Y)): expr6(->X) ">" expr7(->Y)
  'rule' expr6(->same_less(X,Y)): expr6(->X) "<=" expr7(->Y)
  'rule' expr6(->same_more(X,Y)): expr6(->X) ">=" expr7(->Y)

'nonterm' expr7(->Expr)
  'rule' expr7(->X): expr8(->X)
  'rule' expr7(->plus(X,Y)): expr7(->X) "+" expr8(->Y)
  'rule' expr7(->minus(X,Y)): expr7(->X) "-" expr8(->Y)

'nonterm' expr8(->Expr)
  'rule' expr8(->X): expr9(->X)
  'rule' expr8(->mult(X,Y)): expr8(->X) "*" expr9(->Y)
  'rule' expr8(->div(X,Y)): expr8(->X) "/" expr9(->Y)
  'rule' expr8(->mod(X,Y)): expr8(->X) "%" expr9(->Y)

'nonterm' expr9(->Expr)
  'rule' expr9(->X): expr10(->X)
  'rule' expr9(->negation(X)): "!" expr9(->X)

'nonterm' expr10(->Expr)
  'rule' expr10(->pp_ident(X)): "++" Ident(->X)
  'rule' expr10(->mm_ident(X)): "--" Ident(->X)
  'rule' expr10(->num(X)): Number(->X)
  'rule' expr10(->ident(X)): Ident(->X)
  'rule' expr10(->tab(X)): tab(->X)
  'rule' expr10(->ident_mm(X)): Ident(->X) "--"
  'rule' expr10(->ident_pp(X)): Ident(->X) "++"
  'rule' expr10(->X): "(" expr(->X) ")"

'token' Ident(->IDENT)
'token' Number(->INT)


'type' Meaning
  const(INT,INT)
  var(INT)
  tb1(INT,INT)
  tb2(INT,INT,INT)
  tb3(INT,INT,INT,INT)
 

'type' Meaning_list
  nil
  meaning_list(Meaning,Meaning_list)

'var' CurrentLevel: INT

'action' DefMeaning(IDENT, Meaning_list)
'condition' HasMeaning(IDENT->Meaning_list)

'action' InitEnv
  'rule' InitEnv: CurrentLevel <- 0

'action' defineConst(ConstList)
       'rule' defineConst(const_list(is(Id, Expr_), Const_List)):
           CurrentLevel -> ThisLevel
           eval_expr(Expr_ -> Value)
           HasMeaning(Id -> Meaning_list)
           DefMeaning(Id, meaning_list(const(Value, ThisLevel), Meaning_list))
           defineConst(Const_List)         
       'rule' defineConst(nil)


'action' defineVar(List)
  'rule' defineVar(list(Did, Did_list)): 
         defineVar(Did_list) 
         defineDid(Did)
  'rule' defineVar(nil)


'action' defineDid(Did)
  'rule' defineDid(did1(tab1(Ident, Expr_))):
           CurrentLevel->ThisLevel
           HasMeaning(Ident-> Meaning_list)
           eval_expr(Expr_ -> Value)
           DefMeaning(Ident,meaning_list(tb1(Value,ThisLevel),Meaning_list))
  'rule' defineDid(did1(tab2(Ident, Expr1, Expr2))):
           CurrentLevel->ThisLevel
           HasMeaning(Ident->Meaning_list)
           eval_expr(Expr1 -> Value1)
           eval_expr(Expr2 -> Value2)
           DefMeaning(Ident,meaning_list(tb2(Value1,Value2,ThisLevel),Meaning_list))
  'rule' defineDid(did1(tab3(Ident, Expr1, Expr2, Expr3))):
           CurrentLevel->ThisLevel
           HasMeaning(Ident->Meaning_list)
           eval_expr(Expr1 -> Value1)
           eval_expr(Expr2 -> Value2)
           eval_expr(Expr3 -> Value3)
           DefMeaning(Ident,meaning_list(tb3(Value1,Value2,Value3,ThisLevel),Meaning_list))
  'rule' defineDid(did2(Ident)):
           CurrentLevel->ThisLevel
           HasMeaning(Ident->Meaning_list)
           DefMeaning(Ident,meaning_list(var(ThisLevel),Meaning_list)) 

'action' eval_expr(Expr -> INT)
        'rule' eval_expr(E -> 0)

-- Genarating Three Address Code -------------------------------------------------------------------------------------------------------
--     Three Address Code  Abstract Syntax ---------------------------------------------------------------------------------------------
'type' TAC_INSTRUCTION_LIST
        nil
        list(TAC_INSTRUCTION, TAC_INSTRUCTION_LIST)

'type' TAC_INSTRUCTION
        two_arguments_operation(OPERATOR, ARGUMENT, ARGUMENT, ARGUMENT)
        one_argument_operation(OPERATOR, ARGUMENT, ARGUMENT)
        table_reading(ARGUMENT, ARGUMENT, ARGUMENT)
        table_writing(ARGUMENT, ARGUMENT, ARGUMENT)
        if_statement(OPERATOR, ARGUMENT, ARGUMENT, ARGUMENT)
        label(ARGUMENT)

'type' OPERATOR
     plus minus multi div mod equal nequal less greater ge le and or not

'type' ARGUMENT
     number(INT)
     identifier(IDENT)
     temp_variable(INT)

--     Translation Methods ------------------------------------------------------------------------------------------------------------

'var' BreakLabel: INT
'var' ContinueLabel: INT
'var' LabelNumber: INT
'var' TempVariableID: INT

/* at the end of the code we are adding special label for all return statements, this label is recognized by 0 value */
'action' generate_three_address_code(Program -> TAC_INSTRUCTION_LIST)
       'rule' generate_three_address_code(program(DL, IL) -> list(label(number(0)), TAC_IL)):
              init_code_generation
              generate_code_from_declarations(DL, nil -> TAC_DL)
              generate_code_from_instructions(IL, TAC_DL -> TAC_IL)

'action' init_code_generation
         'rule' init_code_generation:
                LabelNumber <- 1
                TempVariableID <- 0
                BreakLabel <- -1
                ContinueLabel <- -1

'action' get_new_temp_variable_id(-> INT)
       'rule' get_new_temp_variable_id(-> ID):
              TempVariableID -> ID
              TempVariableID <- ( ID + 1)

'action' get_new_label_number(->INT)
       'rule' get_new_label_number(->LN):
              LabelNumber -> LN
              LabelNumber <- ( LN + 1)

'action' create_temp_variable(-> ARGUMENT)
        'rule' create_temp_variable(-> temp_variable(ID)):
               get_new_temp_variable_id(->ID)
        
 
'action' generate_code_from_declarations(DeclarationList, TAC_INSTRUCTION_LIST -> TAC_INSTRUCTION_LIST)
        /*Implement me*/
        'rule' generate_code_from_declarations(DL, TAC_IL-> TAC_IL)

'action' generate_code_from_instructions(InstructionList, TAC_INSTRUCTION_LIST -> TAC_INSTRUCTION_LIST)
        'rule' generate_code_from_instructions(instruction_list(Ins,InsList), TAC_IL -> NEW_TAC_IL):
               generate_code_from_instruction(Ins, TAC_IL -> UPDATED_TAC_IL)
               generate_code_from_instructions(InsList, UPDATED_TAC_IL -> NEW_TAC_IL)
        'rule' generate_code_from_instructions(nil, TAC_IL -> TAC_IL)

'action' generate_code_from_instruction(Instruction, TAC_INSTRUCTION_LIST -> TAC_INSTRUCTION_LIST)
        'rule' generate_code_from_instruction(if(EXPR, INSTR), TAC_IL -> NEW_TAC_IL):
               generate_code_for_expression(negation(EXPR), TAC_IL -> U_TAC_IL, TMP_VARIABLE)
               get_new_label_number(->LN)
               where(TAC_INSTRUCTION_LIST'list(if_statement(and, TMP_VARIABLE, number(1), number(LN)), U_TAC_IL) -> UU_TAC_IL)
               generate_code_from_instruction(INSTR, UU_TAC_IL -> UUU_TAC_IL)
               where(TAC_INSTRUCTION_LIST'list(label(number(LN)), UUU_TAC_IL) -> NEW_TAC_IL)

        'rule' generate_code_from_instruction(if_else(EXPR, INSTR1, INSTR2), TAC_IL -> NEW_TAC_IL)
               get_new_label_number(->LN1)
               get_new_label_number(->LN2)
               generate_code_for_expression(negation(EXPR), TAC_IL -> U_TAC_IL, TMP_VARIABLE)
               where(TAC_INSTRUCTION_LIST'list(if_statement(and, TMP_VARIABLE, number(1), number(LN1)), U_TAC_IL) -> UU_TAC_IL)
               generate_code_from_instruction(INSTR1, UU_TAC_IL -> UUU_TAC_IL)
               where(TAC_INSTRUCTION_LIST'list(if_statement(equal, number(0), number(0), number(LN2)), UUU_TAC_IL) -> UUUU_TAC_IL)
               where(TAC_INSTRUCTION_LIST'list(label(number(LN1)), UUUU_TAC_IL) -> UUUUU_TAC_IL)
               generate_code_from_instruction(INSTR1, UUUUU_TAC_IL -> UUUUUU_TAC_IL)
               where(TAC_INSTRUCTION_LIST'list(label(number(LN2)), UUUUUU_TAC_IL) -> NEW_TAC_IL)

        'rule' generate_code_from_instruction(while(EXPR, INSTR), TAC_IL -> NEW_TAC_IL)
               get_new_label_number(-> LN1)
               get_new_label_number(-> LN2)
               BreakLabel -> OldBreakLabel 
               BreakLabel <- LN2
               ContinueLabel -> OldContinueLabel
               ContinueLabel <- LN1
               where(TAC_INSTRUCTION_LIST'list(label(number(LN1)), TAC_IL) -> U_TAC_IL)
               generate_code_for_expression(negation(EXPR), U_TAC_IL -> UU_TAC_IL, TMP_VARIABLE)
               where(TAC_INSTRUCTION_LIST'list(if_statement(and, TMP_VARIABLE, number(1), number(LN2)), UU_TAC_IL) -> UUU_TAC_IL)
               generate_code_from_instruction(INSTR, UUU_TAC_IL -> UUUU_TAC_IL)
               where(TAC_INSTRUCTION_LIST'list(if_statement(equal, number(0), number(0), number(LN1)), UUUU_TAC_IL) -> UUUUU_TAC_IL)
               where(TAC_INSTRUCTION_LIST'list(label(number(LN2)), UUUUU_TAC_IL) -> NEW_TAC_IL)
               BreakLabel <- OldBreakLabel
               ContinueLabel <- OldContinueLabel
        
        'rule' generate_code_from_instruction(break, TAC_IL ->
                   list(if_statement(equal, number(0), number(0), number(BL)), TAC_IL))
               BreakLabel -> BL

        'rule' generate_code_from_instruction(continue, TAC_IL ->
                  list(if_statement(equal, number(0), number(0), number(CL)), TAC_IL))
               ContinueLabel -> CL
               
        'rule' generate_code_from_instruction(nil, TAC_IL -> TAC_IL)
        'rule' generate_code_from_instruction(label(I), TAC_IL -> list(label(identifier(I)), TAC_IL))
        'rule' generate_code_from_instruction(goto(I), TAC_IL -> list(if_statement(equal, number(0), number(0), identifier(I)) , TAC_IL))
        /* we will add a special label at the end of the code , witch will be label for all returns in code */
        'rule' generate_code_from_instruction(return, TAC_IL -> list(if_statement(equal, number(0), number(0), number(0)), TAC_IL))
        'rule' generate_code_from_instruction(block(DL, IL), TAC_IL -> NEW_TAC_IL):
               generate_code_from_declarations(DL, TAC_IL -> UPDATED_TAC_IL)
               generate_code_from_instructions(IL, UPDATED_TAC_IL -> NEW_TAC_IL)
        'rule' generate_code_from_instruction(expr(E), TAC_IL -> NEW_TAC_IL):
               generate_code_for_expression(E, TAC_IL -> NEW_TAC_IL, EXPR_VALUE) /*EXPR_VALUE - variable which holds results for this expr */

'action' generate_code_for_expression(Expr, TAC_INSTRUCTION_LIST -> TAC_INSTRUCTION_LIST, ARGUMENT)
       'rule' generate_code_for_expression(coma(EXPR1, EXPR2), TAC_IL -> NEW_TAC_IL, EXPR_VALUE):
              generate_code_for_expression(EXPR1, TAC_IL -> U_TAC_IL, _)
              generate_code_for_expression(EXPR2, U_TAC_IL -> NEW_TAC_IL, EXPR_VALUE)
       'rule' generate_code_for_expression(is(Id, EXPR2), TAC_IL -> 
                 list(two_arguments_operation(plus, identifier(Id), EXPR_VALUE, number(0)), NEW_TAC_IL) , identifier(Id)):
              generate_code_for_expression(EXPR2, TAC_IL -> NEW_TAC_IL, EXPR_VALUE)
       'rule' generate_code_for_expression(or(EXPR3, EXPR4), TAC_IL -> 
                  list(two_arguments_operation(or, TMP_VARIABLE, EXPR3_VALUE, EXPR4_VALUE), NEW_TAC_IL), TMP_VARIABLE):
              generate_code_for_expression(EXPR3, TAC_IL -> UPDATED_TAC_IL, EXPR3_VALUE)
              generate_code_for_expression(EXPR4, UPDATED_TAC_IL -> NEW_TAC_IL, EXPR4_VALUE)
              create_temp_variable(-> TMP_VARIABLE)
       'rule' generate_code_for_expression(and(EXPR4, EXPR5), TAC_IL -> 
                  list(two_arguments_operation(and, TMP_VARIABLE, EXPR4_VALUE, EXPR5_VALUE), NEW_TAC_IL), TMP_VARIABLE):
              generate_code_for_expression(EXPR4, TAC_IL -> UPDATED_TAC_IL, EXPR4_VALUE)
              generate_code_for_expression(EXPR5, UPDATED_TAC_IL -> NEW_TAC_IL, EXPR5_VALUE)
              create_temp_variable(-> TMP_VARIABLE)
       'rule' generate_code_for_expression(same(EXPR5, EXPR6), TAC_IL -> 
                  list(two_arguments_operation(equal, TMP_VARIABLE, EXPR5_VALUE, EXPR6_VALUE), NEW_TAC_IL), TMP_VARIABLE):
              generate_code_for_expression(EXPR5, TAC_IL -> UPDATED_TAC_IL, EXPR5_VALUE)
              generate_code_for_expression(EXPR6, UPDATED_TAC_IL -> NEW_TAC_IL, EXPR6_VALUE)
              create_temp_variable(-> TMP_VARIABLE)
       'rule' generate_code_for_expression(not_same(EXPR5, EXPR6), TAC_IL -> 
                  list(two_arguments_operation(nequal, TMP_VARIABLE, EXPR5_VALUE, EXPR6_VALUE), NEW_TAC_IL), TMP_VARIABLE):
              generate_code_for_expression(EXPR5, TAC_IL -> UPDATED_TAC_IL, EXPR5_VALUE)
              generate_code_for_expression(EXPR6, UPDATED_TAC_IL -> NEW_TAC_IL, EXPR6_VALUE)
              create_temp_variable(-> TMP_VARIABLE)
       'rule' generate_code_for_expression(less(EXPR6, EXPR7), TAC_IL -> 
                  list(two_arguments_operation(less, TMP_VARIABLE, EXPR6_VALUE, EXPR7_VALUE), NEW_TAC_IL), TMP_VARIABLE):
              generate_code_for_expression(EXPR6, TAC_IL -> UPDATED_TAC_IL, EXPR6_VALUE)
              generate_code_for_expression(EXPR7, UPDATED_TAC_IL -> NEW_TAC_IL, EXPR7_VALUE)
              create_temp_variable(-> TMP_VARIABLE)
       'rule' generate_code_for_expression(more(EXPR6, EXPR7), TAC_IL -> 
                  list(two_arguments_operation(greater, TMP_VARIABLE, EXPR6_VALUE, EXPR7_VALUE), NEW_TAC_IL), TMP_VARIABLE):
              generate_code_for_expression(EXPR6, TAC_IL -> UPDATED_TAC_IL, EXPR6_VALUE)
              generate_code_for_expression(EXPR7, UPDATED_TAC_IL -> NEW_TAC_IL, EXPR7_VALUE)
              create_temp_variable(-> TMP_VARIABLE)
       'rule' generate_code_for_expression(same_less(EXPR6, EXPR7), TAC_IL -> 
                  list(two_arguments_operation(le, TMP_VARIABLE, EXPR6_VALUE, EXPR7_VALUE), NEW_TAC_IL), TMP_VARIABLE):
              generate_code_for_expression(EXPR6, TAC_IL -> UPDATED_TAC_IL, EXPR6_VALUE)
              generate_code_for_expression(EXPR7, UPDATED_TAC_IL -> NEW_TAC_IL, EXPR7_VALUE)
              create_temp_variable(-> TMP_VARIABLE)
       'rule' generate_code_for_expression(same_more(EXPR6, EXPR7), TAC_IL -> 
                  list(two_arguments_operation(ge, TMP_VARIABLE, EXPR6_VALUE, EXPR7_VALUE), NEW_TAC_IL), TMP_VARIABLE):
              generate_code_for_expression(EXPR6, TAC_IL -> UPDATED_TAC_IL, EXPR6_VALUE)
              generate_code_for_expression(EXPR7, UPDATED_TAC_IL -> NEW_TAC_IL, EXPR7_VALUE)
              create_temp_variable(-> TMP_VARIABLE)
       'rule' generate_code_for_expression(plus(EXPR7, EXPR8), TAC_IL -> 
                  list(two_arguments_operation(plus, TMP_VARIABLE, EXPR7_VALUE, EXPR8_VALUE), NEW_TAC_IL), TMP_VARIABLE):
              generate_code_for_expression(EXPR7, TAC_IL -> UPDATED_TAC_IL, EXPR7_VALUE)
              generate_code_for_expression(EXPR8, UPDATED_TAC_IL -> NEW_TAC_IL, EXPR8_VALUE)
              create_temp_variable(-> TMP_VARIABLE)
       'rule' generate_code_for_expression(minus(EXPR7, EXPR8), TAC_IL -> 
                  list(two_arguments_operation(minus, TMP_VARIABLE, EXPR7_VALUE, EXPR8_VALUE), NEW_TAC_IL), TMP_VARIABLE):
              generate_code_for_expression(EXPR7, TAC_IL -> UPDATED_TAC_IL, EXPR7_VALUE)
              generate_code_for_expression(EXPR8, UPDATED_TAC_IL -> NEW_TAC_IL, EXPR8_VALUE)
              create_temp_variable(-> TMP_VARIABLE)
       'rule' generate_code_for_expression(mult(EXPR8, EXPR9), TAC_IL -> 
                  list(two_arguments_operation(multi, TMP_VARIABLE, EXPR8_VALUE, EXPR9_VALUE), NEW_TAC_IL), TMP_VARIABLE):
              generate_code_for_expression(EXPR8, TAC_IL -> UPDATED_TAC_IL, EXPR8_VALUE)
              generate_code_for_expression(EXPR9, UPDATED_TAC_IL -> NEW_TAC_IL, EXPR9_VALUE)
              create_temp_variable(-> TMP_VARIABLE)
       'rule' generate_code_for_expression(div(EXPR8, EXPR9), TAC_IL -> 
                  list(two_arguments_operation(div, TMP_VARIABLE, EXPR8_VALUE, EXPR9_VALUE), NEW_TAC_IL), TMP_VARIABLE):
              generate_code_for_expression(EXPR8, TAC_IL -> UPDATED_TAC_IL, EXPR8_VALUE)
              generate_code_for_expression(EXPR9, UPDATED_TAC_IL -> NEW_TAC_IL, EXPR9_VALUE)
              create_temp_variable(-> TMP_VARIABLE)
       'rule' generate_code_for_expression(mod(EXPR8, EXPR9), TAC_IL -> 
                  list(two_arguments_operation(mod, TMP_VARIABLE, EXPR8_VALUE, EXPR9_VALUE), NEW_TAC_IL), TMP_VARIABLE):
              generate_code_for_expression(EXPR8, TAC_IL -> UPDATED_TAC_IL, EXPR8_VALUE)
              generate_code_for_expression(EXPR9, UPDATED_TAC_IL -> NEW_TAC_IL, EXPR9_VALUE)
              create_temp_variable(-> TMP_VARIABLE)
       'rule' generate_code_for_expression(negation(EXPR9), TAC_IL -> 
                  list(one_argument_operation(not, TMP_VARIABLE, EXPR9_VALUE), NEW_TAC_IL), TMP_VARIABLE):
              generate_code_for_expression(EXPR9, TAC_IL -> NEW_TAC_IL, EXPR9_VALUE)
              create_temp_variable(-> TMP_VARIABLE)
       'rule' generate_code_for_expression(num(N), TAC_IL -> TAC_IL, number(N))
       'rule' generate_code_for_expression(ident(I), TAC_IL -> TAC_IL, identifier(I))
       'rule' generate_code_for_expression(pp_ident(I), TAC_IL -> NEW_TAC_IL,  TMP_VARIABLE):
              where(TAC_INSTRUCTION_LIST'list(two_arguments_operation(plus, identifier(I), identifier(I), number(1)), TAC_IL) -> U_TAC_IL)
              create_temp_variable(->TMP_VARIABLE)
              where(TAC_INSTRUCTION_LIST'list(two_arguments_operation(plus, TMP_VARIABLE, identifier(I), number(0)), U_TAC_IL) -> NEW_TAC_IL)
       'rule' generate_code_for_expression(mm_ident(I), TAC_IL -> NEW_TAC_IL,  TMP_VARIABLE):
              where(TAC_INSTRUCTION_LIST'list(two_arguments_operation(minus, identifier(I), identifier(I), number(1)), TAC_IL) -> U_TAC_IL)
              create_temp_variable(->TMP_VARIABLE)
              where(TAC_INSTRUCTION_LIST'list(two_arguments_operation(plus, TMP_VARIABLE, identifier(I), number(0)), U_TAC_IL) -> NEW_TAC_IL)
       'rule' generate_code_for_expression(ident_pp(I), TAC_IL -> NEW_TAC_IL,  TMP_VARIABLE):
              create_temp_variable(->TMP_VARIABLE)
              where(TAC_INSTRUCTION_LIST'list(two_arguments_operation(plus, TMP_VARIABLE, identifier(I), number(0)), TAC_IL) -> U_TAC_IL)
              where(TAC_INSTRUCTION_LIST'list(two_arguments_operation(plus, identifier(I), identifier(I), number(1)), U_TAC_IL) -> NEW_TAC_IL)
       'rule' generate_code_for_expression(ident_mm(I), TAC_IL -> NEW_TAC_IL,  TMP_VARIABLE):
              create_temp_variable(->TMP_VARIABLE)
              where(TAC_INSTRUCTION_LIST'list(two_arguments_operation(plus, TMP_VARIABLE, identifier(I), number(0)), TAC_IL) -> U_TAC_IL)
              where(TAC_INSTRUCTION_LIST'list(two_arguments_operation(minus, identifier(I), identifier(I), number(1)), U_TAC_IL) -> NEW_TAC_IL)
       /* implement rules for tables */


