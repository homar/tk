'root' program(->P)
       generate_three_address_code(P -> TAC)
       reverse_tac_instructions_list(nil, TAC -> RTAC)
       create_label_row_number_map(RTAC, 0, nil -> L_N_MAP)
       print_tac(RTAC, 0, L_N_MAP)

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
  list(Expr, ConstList)

'type' List
  nil
  list(Expr, List)

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
  'rule' const_list(->list(X,nil)):const_object(->X)
  'rule' const_list(->list(X,Y)): const_object(->X) "," const_list(->Y)
   

'nonterm' const_object(->Expr)
  'rule' const_object(->is(X,Y)): Ident(->X) "=" expr2(->Y)

'nonterm' list(->List)
  'rule' list(->list(X, nil)): declaration_element(->X)
  'rule' list(->list(X,Y)): declaration_element(->X) "," list(->Y)

'nonterm' declaration_element(->Expr)
  'rule' declaration_element(->tab(X)): tab(->X)
  'rule' declaration_element(->ident(X)): Ident(->X)
  'rule' declaration_element(->is(X,Y)): Ident(->X) "=" expr2(->Y)

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
-- Semantic checking -----------------------------------------------------------------------------------------------------------------

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
'action' ErrorI (STRING, IDENT, STRING, POS)

'action' check_declaration_list(DeclarationList)
  'rule' check_declaration_list(declaration_list(Declaration_,Declaration_List)): 
            check_declaration(Declaration_)
            check_declaration_list(Declaration_List)
  'rule' check_declaration_list(nil)

'action' check_instruction_list(InstructionList)
  'rule' check_instruction_list(instruction_list(Instruction_,Instruction_List)): 
            check_instruction(Instruction_)
            check_instruction_list(Instruction_List)
  'rule' check_instruction_list(nil)

'action' check_declaration(Declaration)
  'rule' check_declaration(const_list(Const_List)):
            check_const_list(Const_List)
  'rule' check_declaration(list(List_)):
            check_list(List_)

'action' check_list(List)
  'rule' check_list(nil)
  'rule' check_list(list(Expr_,List_)):
            check_expr_declaration(Expr_)
            check_list(List_)

'action' check_const_list(ConstList)
  'rule' check_const_list(nil)
  'rule' check_const_list(list(Expr_,Const_List)):
            check_expr_declaration(Expr_)
            check_const_list(Const_List)

'action' check_expr_declaration(Expr)
  'rule' check_expr_declaration(ident(Id)):
            check_id(Id)
  'rule' check_expr_declaration(is(Id,Expr_)):
            check_id(Id)

'action' check_id(IDENT)
  'rule' check_id(Id):
            HasMeaning(Id->MList)
            check_meaning_list(MList)
  'rule' check_id(Id):
            HasMeaning(Id->MList)
            CurrentLevel->ThisLev
            DefMeaning(Id,meaning_list(const(ThisLev,0),MList))
  'rule' check_id(Id):
            CurrentLevel->ThisLev
            DefMeaning(Id,meaning_list(const(ThisLev,0),nil))

'condition' check_meaning_list(Meaning_list)
  'rule' check_meaning_list(meaning_list(Meaning_,Meaning_List))
            check_meaning(Meaning_)
  'rule' check_meaning_list(nil):
            

'condition' check_meaning(Meaning)
  'rule' check_meaning(const(Level,B)):
            CurrentLevel->ThisLevel
            eq(Level,ThisLevel)
            my_print("powtorna deklaracja")
  'rule' check_meaning(var(Level)):
            CurrentLevel->ThisLevel
            eq(Level,ThisLevel)
            my_print("powtorna deklaracja")
  'rule' check_meaning(tb1(Level,X)):
            CurrentLevel->ThisLevel
            eq(Level,ThisLevel)
            my_print("powtorna deklaracja")
  'rule' check_meaning(tb2(Level,X,Y)):
            CurrentLevel->ThisLevel
            eq(Level,ThisLevel)
            my_print("powtorna deklaracja")
  'rule' check_meaning(tb3(Level,X,Y,Z)):
            CurrentLevel->ThisLevel
            eq(Level,ThisLevel)
            my_print("powtorna deklaracja")

            

'action' check_instruction(Instruction)
  'rule' check_instruction(expr(Expr_)):
            check_expr(Expr_)
  'rule' check_instruction(label(Id)): 
            check_label(Id)
  'rule' check_instruction(block(Declaration_List,Instruction_List)):
            CurrentLevel->N
            CurrentLevel<-N+1
            check_declaration_list(Declaration_List)
            check_instruction_list(Instruction_List)
            CurrentLevel<-N

'action' check_expr(Expr)
  'rule' check_expr(ident(Id)):
            check_id_instr(Id)
  'rule' check_expr(coma(Expr1,Expr2)):
            check_expr(Expr1)
            check_expr(Expr2)
  'rule' check_expr(is(Id,Expr2)):
            check_id_instr(Id)
            check_expr(Expr2)
 'rule' check_expr(or(Expr1,Expr2)):
            check_expr(Expr1)
            check_expr(Expr2)
 'rule' check_expr(and(Expr1,Expr2)):
            check_expr(Expr1)
            check_expr(Expr2)
 'rule' check_expr(same(Expr1,Expr2)):
            check_expr(Expr1)
            check_expr(Expr2)
 'rule' check_expr(not_same(Expr1,Expr2)):
            check_expr(Expr1)
            check_expr(Expr2)
 'rule' check_expr(less(Expr1,Expr2)):
            check_expr(Expr1)
            check_expr(Expr2)
 'rule' check_expr(more(Expr1,Expr2)):
            check_expr(Expr1)
            check_expr(Expr2)
 'rule' check_expr(same_less(Expr1,Expr2)):
            check_expr(Expr1)
            check_expr(Expr2)
 'rule' check_expr(same_more(Expr1,Expr2)):
            check_expr(Expr1)
            check_expr(Expr2)
 'rule' check_expr(plus(Expr1,Expr2)):
            check_expr(Expr1)
            check_expr(Expr2)
 'rule' check_expr(minus(Expr1,Expr2)):
            check_expr(Expr1)
            check_expr(Expr2)
 'rule' check_expr(mult(Expr1,Expr2)):
            check_expr(Expr1)
            check_expr(Expr2)
 'rule' check_expr(div(Expr1,Expr2)):
            check_expr(Expr1)
            check_expr(Expr2)
 'rule' check_expr(mod(Expr1,Expr2)):
            check_expr(Expr1)
            check_expr(Expr2)
 'rule' check_expr(negation(Expr2)):
            check_expr(Expr2)
 'rule' check_expr(ident_mm(ID)):
            check_id_instr(ID)
 'rule' check_expr(ident_pp(ID)):
            check_id_instr(ID)
 'rule' check_expr(mm_ident(ID)):
            check_id_instr(ID)
 'rule' check_expr(pp_ident(ID)):
            check_id_instr(ID)











 
'action' check_id_instr(IDENT)
  'rule' check_id_instr(Id):
            HasMeaning(Id->MList)
            check_id_meaning_list(MList)
  'rule' check_id_instr(Id):
            my_print("zmienna nie zadeklarowana")

'condition' check_id_meaning_list(Meaning_list)
  'rule' check_id_meaning_list(meaning_list(Meaning_,Meaning_List)):
           (|check_id_meaning(Meaning_)
            || 
            check_id_meaning_list(Meaning_List)
            |)

'condition' check_id_meaning(Meaning)
  'rule' check_id_meaning(const(Level,B)):
            CurrentLevel->ThisLevel
            le(Level,ThisLevel)
  'rule' check_id_meaning(var(Level)):
            CurrentLevel->ThisLevel
            le(Level,ThisLevel)
  'rule' check_id_meaning(tb1(Level,X)):
            CurrentLevel->ThisLevel
            le(Level,ThisLevel)
  'rule' check_id_meaning(tb2(Level,X,T)):
            CurrentLevel->ThisLevel
            le(Level,ThisLevel)
  'rule' check_id_meaning(tb3(Level,X,Y,R)):
            CurrentLevel->ThisLevel
            le(Level,ThisLevel)
            
'action' check_label(IDENT)
  'rule' check_label(Id):
            HasMeaning(Id->MList)
            my_print("taki label juz istnieje")
  'rule' check_label(Id):
            CurrentLevel->ThisLev
            DefMeaning(Id, meaning_list(tb1(ThisLev,0),nil))


'action' InitEnv
  'rule' InitEnv: CurrentLevel <- 0

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
       'rule' generate_three_address_code(program(DL, IL) -> 
                   list(one_argument_operation(not, temp_variable(0), number(0)), list(label(number(0)), TAC_IL))):
              init_code_generation
                InitEnv
                check_declaration_list(DL)
                check_instruction_list(IL)
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
        'rule' generate_code_from_declarations(declaration_list(DECL, DECLL), TAC_DL-> NEW_TAC_DL):
               generate_code_from_declaration(DECL, TAC_DL -> U_TAC_DL)
               generate_code_from_declarations(DECLL, U_TAC_DL -> NEW_TAC_DL)
        'rule' generate_code_from_declarations(nil, TAC_DL -> TAC_DL)

'action' generate_code_from_declaration(Declaration, TAC_INSTRUCTION_LIST -> TAC_INSTRUCTION_LIST)
        'rule' generate_code_from_declaration(list(LIST), TAC_DL -> NEW_TAC_DL):
               generate_code_from_declaration_list(LIST, TAC_DL -> NEW_TAC_DL)
        'rule' generate_code_from_declaration(const_list(CLIST), TAC_DL -> NEW_TAC_DL):
               generate_code_from_declaration_const_list(CLIST, TAC_DL -> NEW_TAC_DL)

'action' generate_code_from_declaration_list(List, TAC_INSTRUCTION_LIST -> TAC_INSTRUCTION_LIST)
        'rule' generate_code_from_declaration_list(list(EXPR, L), TAC_DL -> NEW_TAC_DL):
               generate_code_for_expression(EXPR, TAC_DL -> U_TAC_DL, _)
               generate_code_from_declaration_list(L, U_TAC_DL -> NEW_TAC_DL)
        'rule' generate_code_from_declaration_list(nil, TAC_DL -> TAC_DL)

'action' generate_code_from_declaration_const_list(ConstList, TAC_INSTRUCTION_LIST -> TAC_INSTRUCTION_LIST)
        'rule' generate_code_from_declaration_const_list(list(EXPR, CL), TAC_DL -> NEW_TAC_DL):
               generate_code_for_expression(EXPR, TAC_DL -> U_TAC_DL, _)
               generate_code_from_declaration_const_list(CL, U_TAC_DL -> NEW_TAC_DL)
        'rule' generate_code_from_declaration_const_list(nil, TAC_DL -> TAC_DL)


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

        'rule' generate_code_from_instruction(if_else(EXPR, INSTR1, INSTR2), TAC_IL -> NEW_TAC_IL):
               get_new_label_number(->LN1)
               get_new_label_number(->LN2)
               generate_code_for_expression(negation(EXPR), TAC_IL -> U_TAC_IL, TMP_VARIABLE)
               where(TAC_INSTRUCTION_LIST'list(if_statement(and, TMP_VARIABLE, number(1), number(LN1)), U_TAC_IL) -> UU_TAC_IL)
               generate_code_from_instruction(INSTR1, UU_TAC_IL -> UUU_TAC_IL)
               where(TAC_INSTRUCTION_LIST'list(if_statement(equal, number(0), number(0), number(LN2)), UUU_TAC_IL) -> UUUU_TAC_IL)
               where(TAC_INSTRUCTION_LIST'list(label(number(LN1)), UUUU_TAC_IL) -> UUUUU_TAC_IL)
               generate_code_from_instruction(INSTR1, UUUUU_TAC_IL -> UUUUUU_TAC_IL)
               where(TAC_INSTRUCTION_LIST'list(label(number(LN2)), UUUUUU_TAC_IL) -> NEW_TAC_IL)

        'rule' generate_code_from_instruction(while(EXPR, INSTR), TAC_IL -> NEW_TAC_IL):
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

        'rule' generate_code_from_instruction(do(INSTR, EXPR), TAC_IL -> NEW_TAC_IL):
               get_new_label_number(-> LN1)
               get_new_label_number(-> LN2)
               BreakLabel -> OldBreakLabel 
               BreakLabel <- LN2
               ContinueLabel -> OldContinueLabel
               ContinueLabel <- LN1
               where(TAC_INSTRUCTION_LIST'list(label(number(LN1)), TAC_IL) -> U_TAC_IL)
               generate_code_from_instruction(INSTR, U_TAC_IL -> UU_TAC_IL)
               generate_code_for_expression(EXPR, UU_TAC_IL -> UUU_TAC_IL, TMP_VARIABLE)
               where(TAC_INSTRUCTION_LIST'list(if_statement(and, TMP_VARIABLE, number(1), number(LN1)), UUU_TAC_IL) -> UUUU_TAC_IL)
               where(TAC_INSTRUCTION_LIST'list(label(number(LN2)), UUUU_TAC_IL) -> NEW_TAC_IL)
               BreakLabel <- OldBreakLabel
               ContinueLabel <- OldContinueLabel
-- ' apostrophe for good syntax highlighting

        'rule' generate_code_from_instruction(for(EXPR1, EXPR2, EXPR3, INSTR), TAC_IL -> NEW_TAC_IL):
               get_new_label_number(-> LN1)
               get_new_label_number(-> LN2)
               BreakLabel -> OldBreakLabel 
               BreakLabel <- LN2
               ContinueLabel -> OldContinueLabel
               ContinueLabel <- LN1
               generate_code_for_expression(EXPR1, TAC_IL -> U_TAC_IL, _)
               where(TAC_INSTRUCTION_LIST'list(label(number(LN1)), U_TAC_IL) -> UU_TAC_IL)
               generate_code_for_expression(negation(EXPR2), UU_TAC_IL -> UUU_TAC_IL, TMP_VARIABLE)
               where(TAC_INSTRUCTION_LIST'list(if_statement(and, TMP_VARIABLE, number(1), number(LN2)), UUU_TAC_IL) -> UUUU_TAC_IL)
               generate_code_from_instruction(INSTR, UUUU_TAC_IL -> UUUUU_TAC_IL)
               generate_code_for_expression(EXPR3, UUUUU_TAC_IL -> UUUUUU_TAC_IL, _)
               where(TAC_INSTRUCTION_LIST'list(if_statement(equal, number(0), number(0), number(LN1)), UUUUUU_TAC_IL) -> UUUUUUU_TAC_IL)
               where(TAC_INSTRUCTION_LIST'list(label(number(LN2)), UUUUUUU_TAC_IL) -> NEW_TAC_IL)
               BreakLabel <- OldBreakLabel
               ContinueLabel <- OldContinueLabel
        
        
        'rule' generate_code_from_instruction(break, TAC_IL ->
                   list(if_statement(equal, number(0), number(0), number(BL)), TAC_IL)):
               BreakLabel -> BL

        'rule' generate_code_from_instruction(continue, TAC_IL ->
                  list(if_statement(equal, number(0), number(0), number(CL)), TAC_IL)):
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
       'rule' generate_code_for_expression(nil, TAC_IL -> TAC_IL, number(0))
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

-- Action Wchich Reverse list of instruction fo future actians --------------------------------------------------------------------------------

'action' reverse_tac_instructions_list(TAC_INSTRUCTION_LIST, TAC_INSTRUCTION_LIST -> TAC_INSTRUCTION_LIST)
       'rule' reverse_tac_instructions_list(NTACL, list(TAC_I, TAC_L) -> RTACL):
              reverse_tac_instructions_list(list(TAC_I, NTACL),  TAC_L ->RTACL)
       'rule' reverse_tac_instructions_list(NTACL, nil -> NTACL)


-- Action which count proper number of rows for jumps and others ------------------------------------------------------------------------------
'type' LABEL_ROW_MAP
    nil
    list(MAP_ENTITY, LABEL_ROW_MAP)

'type' MAP_ENTITY
    entity(ARGUMENT, INT)
    
'action' create_label_row_number_map(TAC_INSTRUCTION_LIST, INT, LABEL_ROW_MAP-> LABEL_ROW_MAP)
       'rule' create_label_row_number_map(list(label(ARG), TACL), ROW_NUMBER, MAP -> NEW_MAP)
              create_label_row_number_map(TACL, ROW_NUMBER, list(entity(ARG, ROW_NUMBER),MAP) -> NEW_MAP)
       'rule' create_label_row_number_map(list(TACI, TACL), ROW_NUMBER, MAP -> NEW_MAP)
              create_label_row_number_map(TACL, ROW_NUMBER + 1, MAP -> NEW_MAP)
       'rule' create_label_row_number_map(nil, ROW_NUMBER, MAP -> MAP)

-- Action To Print Three Address Code To Output -----------------------------------------------------------------------------------------------
'action' print_tac(TAC_INSTRUCTION_LIST, INT, LABEL_ROW_MAP)
       'rule' print_tac(list(label(_), TACL), ROW_NUMBER, LABEL_ROW_MAP)
              print_tac(TACL, ROW_NUMBER, LABEL_ROW_MAP)
       'rule' print_tac(list(TACI, TACL),ROW_NUMBER, LABEL_ROW_MAP)
              my_int_print(ROW_NUMBER) my_print(" ")
              print_tac_instruction(TACI, LABEL_ROW_MAP)
              my_println(" ")
              print_tac(TACL, ROW_NUMBER + 1, LABEL_ROW_MAP)
       'rule' print_tac(nil, ROW_NUMBER, LABEL_ROW_MAP)

'action' print_tac_instruction(TAC_INSTRUCTION, LABEL_ROW_MAP)
       'rule' print_tac_instruction(two_arguments_operation(O, A1, A2, A3), LM):
              print_operator(O) my_print(" ") print_argument(A1) my_print(" ") print_argument(A2) my_print(" ") print_argument(A3)
       'rule' print_tac_instruction(one_argument_operation(O, A1, A2), LM):     
              print_operator(O) my_print(" ") print_argument(A1) my_print(" ") print_argument(A2)
       'rule' print_tac_instruction(table_reading(A1, A2, A3), LM):
              my_print("tout") my_print(" ") print_argument(A1) my_print(" ") print_argument(A2) my_print(" ") print_argument(A3)
       'rule' print_tac_instruction(table_writing(A1, A2, A3), LM):
              my_print("tin") my_print(" ") print_argument(A1) my_print(" ") print_argument(A2) my_print(" ") print_argument(A3)
       'rule' print_tac_instruction(if_statement(O, A1, A2, A3), LM):
              get_label_row(A3, LM -> N)
              my_print("if") print_operator(O) my_print(" ") print_argument(A1) my_print(" ") print_argument(A2)  my_print(" ") my_int_print(N)

'action' print_operator(OPERATOR)
       'rule' print_operator(plus): my_print("+")
       'rule' print_operator(minus): my_print("-")
       'rule' print_operator(multi): my_print("*")
       'rule' print_operator(div): my_print("/")
       'rule' print_operator(mod): my_print("%")
       'rule' print_operator(equal): my_print("==")
       'rule' print_operator(nequal): my_print("!=")
       'rule' print_operator(less): my_print("<")
       'rule' print_operator(greater): my_print(">")
       'rule' print_operator(ge): my_print(">=")
       'rule' print_operator(le): my_print("<=")
       'rule' print_operator(and): my_print("&&")
       'rule' print_operator(or): my_print("||")
       'rule' print_operator(not): my_print("!")

'action' print_argument(ARGUMENT)
       'rule' print_argument(number(N)): my_int_print(N)
       'rule' print_argument(identifier(I)): id_to_string(I -> S) my_print(S)
       'rule' print_argument(temp_variable(N)): my_print("mem") my_int_print(N)

'action' get_label_row(ARGUMENT, LABEL_ROW_MAP -> INT)
       'rule' get_label_row(ARG1, list(entity(ARG2, N), LM) -> N):
              eq(ARG1, ARG2)
       'rule' get_label_row(ARG, list(_, LM) -> N):
              get_label_row(ARG, LM -> N)

--Interface to writing method -----------------------------------------------------------------------------------------------------------------

'action' my_print(STRING)
'action' my_println(STRING)
'action' my_int_print(INT)
'action' my_int_println(INT)
'action' id_to_string(IDENT -> STRING)
              

