#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  void semant(Class_ class_);
  int semant_errors_;
  ostream &error_stream_;
  Classes classes_;
  SymbolTable<Symbol, SymbolTable<Symbol, Feature_class>> basic_class_tables_;
  SymbolTable<Symbol, SymbolTable<Symbol, Feature_class>> class_tables_;
  SymbolTable<Symbol, SymbolTable<Symbol, Entry>> symbol_tables_;
  SymbolTable<Symbol, Entry> class_hierarchy_;

public:
  ClassTable(Classes classes);

  int errors() { return semant_errors_; }
  ostream &semant_error();
  ostream &semant_error(Class_ c);
  ostream &semant_error(Symbol filename, tree_node *t);
  Symbol current_file_name_;
  Symbol current_class_name_;

  void install_basic_classes();
  void install_user_defined_classes();
  void check_inheritance();
  void check_type();
  void check_expr(Expression_class *expr);
  void check_new(new__class *new_);
  void check_block(block_class *block);
  void check_assign(assign_class *assign);
  void check_object(object_class *object);
  void check_dispatch(dispatch_class *dispatch);
  void check_int_const(int_const_class *int_const);
  void check_bool_const(bool_const_class *bool_const);
  void check_string_const(string_const_class *string_const);
  void check_cond(cond_class *cond);
  void check_let(let_class *let);
  void check_sub(sub_class *sub);
  void check_loop(loop_class *loop);
  void check_plus(plus_class *plus);
  void check_eq(eq_class *eq);
};


#endif

