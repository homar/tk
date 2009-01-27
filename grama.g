'root' program(->B)

'type' IDENT
'type' NUM

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
  is(IDENT, Expr2)

'type' List
  nil
  list(List,Did)

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

'type' Block
  block(DeclarationList,InstructionList)

'type' Instruction
  instruction_open(OpenStatement)
  instruction_close(ClosedStatement)

'type' OpenStatement
  if(Expr,Instruction)
  if_else(Expr,ClosedStatement,OpenStatement)
  loop_open(LoopHeader,OpenStatement)

'type' ClosedStatement
  simple_statement(SimpleStatement)
  if_else(Expr,ClosedStatement,ClosedStatement)
  do(Expr,Instruction,Expr)
  loop_closed(LoopHeader,ClosedStatement)

'type' LoopHeader
  while(Expr)
  for(ExprFor,ExprFor,ExprFor)

'type' SimpleStatement
  nil
  goto(IDENT)
  simple_statement(IDENT)
  return(Expr)
  expr(Expr)
  block(Block)

'type' ExprFor
  nil
  expr(Expr)

'type' Expr
  expr(Expr2)
  coma(Expr,Expr2)

'type' Expr2
  expr(Expr3) 
  is(IDENT,Expr2)

'type' Expr3
  expr(Expr4)
  or(Expr3,Expr4)

'type' Expr4
  expr(Expr5)
  and(Expr4,Expr5)

'type' Expr5
  expr(Expr6)
  same(Expr5,Expr6)
  not_same(Expr5,Expr6)

'type' Expr6
  expr(Expr7)
  less(Expr6,Expr7)
  more(Expr6,Expr7)
  same_less(Expr6,Expr7)
  same_more(Expr6,Expr7)

'type' Expr7
  expr(Expr8)
  plus(Expr7,Expr8)
  minus(Expr7,Expr8)

'type' Expr8
  expr(Expr9)
  mult(Expr8,Expr9)
  div(Expr8,Expr9)
  mod(Expr8,Expr9)

'type' Expr9
  pp_ident(IDENT)
  mm_ident(IDENT)
  num(NUM)
  ident(IDENT)
  tab(Tab)
  ident_mm(IDENT)
  ident_pp(IDENT)
  expr(Expr)


'nonterm' program(->Program)
  'rule' program(->program(X,Y)): declaration_list(->X) instruction_list(->Y) 

'nonterm' declaration_list(->DeclarationList)
  'rule' declaration_list(->nil):
  'rule' declaration_list(->declaration_list(X,Y)): declaration(->X) declaration_list(->Y)

'nonterm' declaration(->Declaration)
  'rule' declaration(->const_list(X)): "const" "int" const_list(->X) ";"
  'rule' declaration(->list(X)): "int" list(->X) ";"

'nonterm' const_list(->ConstList)
  'rule' const_list(->is(X,Y)): Ident(->X) "=" expr2(->Y)
  'rule' const_list(->const_list(X,Y,Z)): const_list(->X) "," Ident(->Y) "=" expr2(->Z)

'nonterm' list(->List)
  'rule' list(->did(X)): did(->X)
  'rule' list(->list(X,Y)): list(->X) "," did(->Y)

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

'nonterm' block(->Block)
  'rule' block(->block(X,Y)): "{" declaration_list(->X) instruction_list(->Y) "}" 

'nonterm' instruction(->Instruction)
  'rule' instruction(->instruction_open(X)): open_statement(->X)
  'rule' instruction(->instruction_close(X)): closed_statement(->X)

'nonterm' open_statement(->OpenStatement)
  'rule' open_statement(->if(X,Y)): "if" "(" expr(->X) ")" instruction(->Y) 
  'rule' open_statement(->if_else(X,Y,Z)): "if" "(" expr(->X) ")" closed_statement(->Y) "else" open_statement(->Z) 
  'rule' open_statement(->loop_open(X,Y)): loop_header(->X) open_statement(->Y)

'nonterm' closed_statement(->ClosedStatement)
  'rule' closed_statement(->simple_statement(X)): simple_statement(->X)
  'rule' closed_statement(->if_else(X,Y,Z)): "if" "(" expr(->X) ")" closed_statement(->Y) "else" closed_statement(->Z) 
  'rule' closed_statement(->do(X,Y,Z)): "do" "(" expr(->X) ")" instruction(->Y) "while" "(" expr(->Z) ")"
  'rule' closed_statement(->loop_closed(X,Y)): loop_header(->X) closed_statement(->Y)

'nonterm' loop_header(->LoopHeader)
  'rule' loop_header(->while(X)): "while" "(" expr(->X) ")"
  'rule' loop_header(->for(X,Y,Z)): "for" "(" expr_for(->X) ";" expr_for(->Y) ";" expr_for(->Z) ")"
   
'nonterm' simple_statement(->SimpleStatement)
  'rule' simple_statement(->nil): "break" ";"
  'rule' simple_statement(->nil): "continue" ";"
  'rule' simple_statement(->goto(X)): "goto" Ident(->X) ";"
  'rule' simple_statement(->simple_statement(X)): Ident(->X) ":"
  'rule' simple_statement(->return(X)): "return" expr(->X) ";"
  'rule' simple_statement(->expr(X)): expr(->X) ";"
  'rule' simple_statement(->nil): ";"
  'rule' simple_statement(->block(X)): block(->X)

'nonterm' expr_for(->ExprFor)
  'rule' expr_for(->nil): 
  'rule' expr_for(->expr(X)): expr(->X)

'nonterm' expr(->Expr)
  'rule' expr(->expr(X)): expr2(->X)
  'rule' expr(->coma(X,Y)): expr(->X) "," expr2(->Y)      

'nonterm' expr2(->Expr2)
  'rule' expr2(->expr(X)): expr3(->X)
  'rule' expr2(->is(X,Y)): Ident(->X) "=" expr2(->Y)

'nonterm' expr3(->Expr3)
  'rule' expr3(->expr(X)): expr4(->X)
  'rule' expr3(->or(X,Y)): expr3(->X) "||" expr4(->Y)

'nonterm' expr4(->Expr4)
  'rule' expr4(->expr(X)): expr5(->X)
  'rule' expr4(->and(X,Y)): expr4(->X) "&&" expr5(->Y)

'nonterm' expr5(->Expr5)
  'rule' expr5(->expr(X)): expr6(->X)
  'rule' expr5(->same(X,Y)): expr5(->X) "==" expr6(->Y)
  'rule' expr5(->not_same(X,Y)): expr5(->X) "!=" expr6(->Y)

'nonterm' expr6(->Expr6)
  'rule' expr6(->expr(X)): expr7(->X)
  'rule' expr6(->less(X,Y)): expr6(->X) "<" expr7(->Y)
  'rule' expr6(->more(X,Y)): expr6(->X) ">" expr7(->Y)
  'rule' expr6(->same_less(X,Y)): expr6(->X) "<=" expr7(->Y)
  'rule' expr6(->same_more(X,Y)): expr6(->X) ">=" expr7(->Y)

'nonterm' expr7(->Expr7)
  'rule' expr7(->expr(X)): expr8(->X)
  'rule' expr7(->plus(X,Y)): expr7(->X) "+" expr8(->Y)
  'rule' expr7(->minus(X,Y)): expr7(->X) "-" expr8(->Y)

'nonterm' expr8(->Expr8)
  'rule' expr8(->expr(X)): expr9(->X)
  'rule' expr8(->mult(X,Y)): expr8(->X) "*" expr9(->Y)
  'rule' expr8(->div(X,Y)): expr8(->X) "/" expr9(->Y)
  'rule' expr8(->mod(X,Y)): expr8(->X) "%" expr9(->Y)

'nonterm' expr9(->Expr9)
  'rule' expr9(->pp_ident(X)): "++" Ident(->X)
  'rule' expr9(->mm_ident(X)): "--" Ident(->X)
  'rule' expr9(->num(X)): Number(->X)
  'rule' expr9(->ident(X)): Ident(->X)
  'rule' expr9(->tab(X)): tab(->X)
  'rule' expr9(->ident_mm(X)): Ident(->X) "--"
  'rule' expr9(->ident_pp(X)): Ident(->X) "++"
  'rule' expr9(->expr(X)): "(" expr(->X) ")"

'token' Ident(->IDENT)
'token' Number(->NUM)


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
           eval_expr(expr(Expr_) -> Value)
           HasMeaning(Id -> Meaning_list)
           DefMeaning(Id, meaning_list(const(Value, ThisLevel), Meaning_list))
           defineConst(Const_List)         
       'rule' defineConst(nil)


'action' defineVar(List)
  'rule' defineVar(list(Did_list,Did)): 
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

'action' analyze(Program)
  'rule' analyze(program(Declaration_List,Instruction_List)):InitEnv an_declaration_list(Declaration_List) an_instruction_list(Instruction_List)
              
'action' an_declaration_list(DeclarationList)
  'rule' an_declaration_list(declaration_list(Declaration_,Declaration_List)):an_declaration(Declaration_) an_declaration_list(Declaration_List)
  'rule' an_declaration_list(nil)

'action' an_declaration(Declaration)
  'rule' an_declaration(const_list(Const_List)):defineConst(Const_List)
  'rule' an_declaration(list(List_)):defineVar(List_)

'action' an_instruction_list(InstructionList)
  'rule' an_instruction_list(instruction_list(Instruction_,Instruction_List)): an_instruction(Instruction_) an_instruction_list(Instruction_List)
  'rule' an_instruction_list(nil):

'action' an_instruction(Instruction)
  'rule' an_instruction(instruction_open(OpenStatement_)):an_open_statement(OpenStatement_)
  'rule' an_instruction(instruction_close(ClosedStatement_)): an_close_statement(ClosedStatement_)

'action' an_open_statement(OpenStatement)
  'rule' an_open_statement(if(Expr_,Instruction_)): an_expr(Expr_) an_instruction(Instruction_)
  'rule' an_open_statement(if_else(Expr_,ClosedStatement_,OpenStatement_)): an_expr(Expr_) an_close_statement(ClosedStatement_) an_open_statement(OpenStatement_)
  'rule' an_open_statement(loop_open(Loop_Header,OpenStatement_)): an_loop_header(Loop_Header) an_open_statement(OpenStatement_)

'action' an_close_statement(ClosedStatement)
  'rule' an_close_statement(simple_statement(Simple_Statement)): an_simple_statement(Simple_Statement)
  'rule' an_close_statement(if_else(Expr_,Closed_Statement1,Closed_Statement2)): an_expr(Expr_) an_close_statement(Closed_Statement1) an_close_statement(Closed_Statement2)
  'rule' an_close_statement(do(Expr_1,Instruction_,Expr_1)): an_expr(Expr_1) an_instruction(Instruction_) an_expr(Expr_1)
  'rule' an_close_statement(loop_closed(Loop_Header,Closed_Statement)): an_loop_header(Loop_Header) an_close_statement(Closed_Statement)

'action' an_loop_header(LoopHeader)
  'rule' an_loop_header(while(Expr_)): an_expr(Expr_)
  'rule' an_loop_header(for(Expr_For,Expr_For,Expr_For)): an_expr_for(Expr_For) an_expr_for(Expr_For) an_expr_for(Expr_For)

'action' an_simple_statement(SimpleStatement)
  'rule' an_simple_statement(nil)
  'rule' an_simple_statement(goto(Ident)): an_ident(Ident)
  'rule' an_simple_statement(simple_statement(Ident)): an_simple_statement(Ident)
  'rule' an_simple_statement(return(Expr_)): an_expr(Expr_)
  'rule' an_simple_statement(expr(Expr_)): an_expr(Expr_)
  'rule' an_simple_statement(block(Block_)): an_block(Block_)

'action' an_expr_for(ExprFor)
  'rule' an_expr_for(nil)
  'rule' an_expr_for(expr(Expr_)):an_expr(Expr_)

'action' an_block(Block)
  'rule' an_block(block(Declaration_List,Instruction_List)): an_declaration_list(Declaration_List) an_instruction_list(Instruction_List)

'action' an_ident(IDENT)
  'rule' an_ident(Ident)

'action' an_expr(Expr)
  'rule' an_expr(expr(Expr2_)): an_expr2(Expr2_)
  'rule' an_expr(coma(Expr_,Expr2_)): an_expr(Expr_) an_expr2(Expr2_)

'action' an_expr2(Expr2)
  'rule' an_expr2(_)
