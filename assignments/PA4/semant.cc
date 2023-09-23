

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}



ClassTable::ClassTable(Classes classes) : semant_errors_(0), error_stream_(cerr), classes_(classes) {

    /* Fill this in */
    basic_class_tables_.enterscope();
    class_tables_.enterscope();
    class_hierarchy_.enterscope();
    symbol_tables_.enterscope();
    install_basic_classes();
    install_user_defined_classes();
    check_inheritance();
    check_type();
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    // Class_ Object_class =
	// class_(Object, 
	//        No_class,
	//        append_Features(
	// 		       append_Features(
	// 				       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
	// 				       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
	// 		       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	//        filename);
    {
        auto *feature_table = new SymbolTable<Symbol, Feature_class>();
        feature_table->enterscope();
        feature_table->addid(cool_abort, method(cool_abort, nil_Formals(), Object, no_expr()));
        feature_table->addid(type_name, method(type_name, nil_Formals(), Str, no_expr()));
        feature_table->addid(copy, method(copy, nil_Formals(), SELF_TYPE, no_expr()));
        basic_class_tables_.addid(Object, feature_table);
        class_tables_.addid(Object, feature_table);
    }

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    // Class_ IO_class = 
	// class_(IO, 
	//        Object,
	//        append_Features(
	// 		       append_Features(
	// 				       append_Features(
	// 						       single_Features(method(out_string, single_Formals(formal(arg, Str)),
	// 									      SELF_TYPE, no_expr())),
	// 						       single_Features(method(out_int, single_Formals(formal(arg, Int)),
	// 									      SELF_TYPE, no_expr()))),
	// 				       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
	// 		       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	//        filename);  
    {
        auto *feature_table = new SymbolTable<Symbol, Feature_class>();
        feature_table->enterscope();
        feature_table->addid(out_string, method(out_string, single_Formals(formal(arg, Str)), SELF_TYPE, no_expr()));
        feature_table->addid(out_int, method(out_int, single_Formals(formal(arg, Int)), SELF_TYPE, no_expr()));
        feature_table->addid(in_string, method(in_string, nil_Formals(), Str, no_expr()));
        feature_table->addid(in_int, method(in_int, nil_Formals(), Int, no_expr()));
        basic_class_tables_.addid(IO, feature_table);
        class_tables_.addid(IO, feature_table);
    }
    class_hierarchy_.addid(IO, Object);

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    // Class_ Int_class =
	// class_(Int, 
	//        Object,
	//        single_Features(attr(val, prim_slot, no_expr())),
	//        filename);
    {
        auto *feature_table = new SymbolTable<Symbol, Feature_class>();
        feature_table->enterscope();
        feature_table->addid(val, attr(val, prim_slot, no_expr()));
        basic_class_tables_.addid(Int, feature_table);
        class_tables_.addid(Int, feature_table);
    }
    class_hierarchy_.addid(Int, Object);

    //
    // Bool also has only the "val" slot.
    //
    // Class_ Bool_class =
	// class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);
    {
        auto *feature_table = new SymbolTable<Symbol, Feature_class>();
        feature_table->enterscope();
        feature_table->addid(val, attr(val, prim_slot, no_expr()));
        basic_class_tables_.addid(Bool, feature_table);
        class_tables_.addid(Bool, feature_table);
    }
    class_hierarchy_.addid(Bool, Object);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    // Class_ Str_class =
	// class_(Str, 
	//        Object,
	//        append_Features(
	// 		       append_Features(
	// 				       append_Features(
	// 						       append_Features(
	// 								       single_Features(attr(val, Int, no_expr())),
	// 								       single_Features(attr(str_field, prim_slot, no_expr()))),
	// 						       single_Features(method(length, nil_Formals(), Int, no_expr()))),
	// 				       single_Features(method(concat, 
	// 							      single_Formals(formal(arg, Str)),
	// 							      Str, 
	// 							      no_expr()))),
	// 		       single_Features(method(substr, 
	// 					      append_Formals(single_Formals(formal(arg, Int)), 
	// 							     single_Formals(formal(arg2, Int))),
	// 					      Str, 
	// 					      no_expr()))),
	//        filename);
    {
        auto *feature_table = new SymbolTable<Symbol, Feature_class>();
        feature_table->enterscope();
        feature_table->addid(val, attr(val, Int, no_expr()));
        feature_table->addid(str_field, attr(str_field, prim_slot, no_expr()));
        feature_table->addid(length, method(length, nil_Formals(), Int, no_expr()));
        feature_table->addid(concat, method(concat, single_Formals(formal(arg, Str)), Str, no_expr()));
        feature_table->addid(substr, method(substr, append_Formals(single_Formals(formal(arg, Int)), single_Formals(formal(arg2, Int))), Str, no_expr()));
        basic_class_tables_.addid(Str, feature_table);
        class_tables_.addid(Str, feature_table);
    }
    class_hierarchy_.addid(Str, Object);
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream_ << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors_++;                            
    return error_stream_;
}

void ClassTable::install_user_defined_classes() {
    for (int i = classes_->first(); classes_->more(i); i = classes_->next(i)) {
        if (auto *class_ = dynamic_cast<class__class *>(classes_->nth(i)); class_ != nullptr) {
            auto *class_name = class_->get_name();
            if (class_tables_.lookup(class_name) != nullptr) {
                semant_error() << class_->get_filename()
                            << ":" << class_->get_line_number()
                            << ": Class " << class_name
                            << " was previously defined.";
                continue;
            }

            auto *features = class_->get_features();
            auto *feature_table = new SymbolTable<Symbol, Feature_class>();
            auto *symbol_table = new SymbolTable<Symbol, Entry>();
            feature_table->enterscope();
            symbol_table->enterscope();
            for (int j = features->first(); features->more(j); j = features->next(j)) {
                auto *feature = features->nth(j);
                if (auto *method = dynamic_cast<method_class *>(feature); method != nullptr) {
                    auto *return_type = method->get_return_type();
                    if (return_type->equal_string(SELF_TYPE->get_string(),SELF_TYPE->get_len())) {
                        symbol_table->addid(method->get_name(), class_name);
                    } else {
                        symbol_table->addid(method->get_name(), return_type);
                    }
                } else if (auto *attr = dynamic_cast<attr_class *>(feature); attr != nullptr) {
                    if (attr->get_name()->equal_string(self->get_string(),self->get_len())) {
                        semant_error() << class_->get_filename()
                                        << ":" << attr->get_line_number()
                                        << ": 'self' cannot be the name of an attribute.\n";
                    }
                    symbol_table->addid(attr->get_name(), attr->get_type_decl());
                }
                auto *feature_name = feature->get_name();
                feature_table->addid(feature_name, feature);
            }
            class_tables_.addid(class_name, feature_table);
            symbol_tables_.addid(class_name, symbol_table);
        }
    }
}

void ClassTable::check_inheritance() {
    for (int i = classes_->first(); classes_->more(i); i = classes_->next(i)) {
        if (auto *class_ = dynamic_cast<class__class *>(classes_->nth(i)); class_ != nullptr) {
            auto *class_name = class_->get_name();
            auto *parent_name = class_->get_parent();
            if (basic_class_tables_.lookup(class_name) != nullptr) { 
                semant_error() << class_->get_filename() << ":" << class_->get_line_number()
                                << ": Redefinition of basic class " << class_name << ".\n";
                continue;
            }
            if (basic_class_tables_.lookup(parent_name) == nullptr && class_tables_.lookup(parent_name) == nullptr) {
                semant_error() << class_->get_filename()
                            << ":" << class_->get_line_number()
                            << ": Class " << class_name
                            << " inherits from an undefined class " << parent_name << ".\n";
                continue;
            }
            class_hierarchy_.addid(class_name, parent_name);
        }
    }
}

void ClassTable::check_type() {
    for (int i = classes_->first(); classes_->more(i); i = classes_->next(i)) {
        if (auto *class_ = dynamic_cast<class__class *>(classes_->nth(i)); class_ != nullptr) {
            current_file_name_ = class_->get_filename();
            current_class_name_ = class_->get_name();
            auto *symbol_table = symbol_tables_.lookup(class_->get_name());

            auto *features = class_->get_features();
            for (int j = features->first(); features->more(j); j = features->next(j)) {
                if (auto *method = dynamic_cast<method_class *>(features->nth(j)); method != nullptr) {
                    symbol_table->enterscope();

                    auto *formals = method->get_formals();
                    for (int k = formals->first(); formals->more(k); k = formals->next(k)) {
                        if (auto *formal = dynamic_cast<formal_class *>(formals->nth(k)); formal != nullptr) {
                            symbol_table->addid(formal->get_name(), formal->get_type_decl());
                        }
                    }

                    auto *return_type = method->get_return_type();
                    if (return_type->equal_string(SELF_TYPE->get_string(),SELF_TYPE->get_len())) {
                        return_type = current_class_name_;
                    }
                    if (class_tables_.lookup(return_type) == nullptr) {
                        semant_error() << class_->get_filename() << ":" << method->get_line_number()
                                        << ": Undefined return type " << return_type
                                        << " in method " << method->get_name()
                                        << ".\n";
                        continue;
                    }

                    auto *expr = method->get_expr();
                    check_expr(expr);
                    auto *inferred_type = expr->get_type();
                    if (inferred_type->equal_string(SELF_TYPE->get_string(),SELF_TYPE->get_len())) {
                        inferred_type = current_class_name_;
                    }
                    while (!inferred_type->equal_string(return_type->get_string(), return_type->get_len())) {
                        auto *parent_type = class_hierarchy_.lookup(inferred_type);
                        if (parent_type == nullptr) {
                            semant_error() << class_->get_filename() << ":" << method->get_line_number()
                                            << ": Inferred return type " << inferred_type
                                            << " of method " << method->get_name()
                                            << " does not conform to declared return type " << return_type
                                            << ".\n";
                            break;
                        }
                        inferred_type = parent_type;
                    }
                    symbol_table->exitscope();
                } else if (auto *attr = dynamic_cast<attr_class *>(features->nth(j)); attr != nullptr) {
                    auto *attr_name = attr->get_name();
                    auto *parent_name = class_hierarchy_.lookup(current_class_name_);
                    while (parent_name != NULL) {
                        auto *features = class_tables_.lookup(parent_name);
                        if (features->lookup(attr_name) != nullptr) {
                            semant_error() << class_->get_filename() << ":" << attr->get_line_number()
                                            << ": Attribute " << attr_name
                                            << " is an attribute of an inherited class.\n";
                            break;
                        }
                        parent_name = class_hierarchy_.lookup(parent_name);
                    }

                    auto *type_decl = attr->get_type_decl();
                    if (class_tables_.lookup(type_decl) == nullptr) {
                        semant_error() << class_->get_filename() << ":" << attr->get_line_number()
                                        << ": Class " << type_decl
                                        << " of attribute " << attr->get_name()
                                        << " is undefined.\n";
                        continue;
                    }

                    auto *init = attr->get_init();
                    if (dynamic_cast<no_expr_class *>(init) != nullptr) {
                        continue;
                    }
                    check_expr(init);
                    auto *inferred_type = init->get_type();
                    auto *parent_type = class_hierarchy_.lookup(inferred_type);
                    while (inferred_type != nullptr && !inferred_type->equal_string(type_decl->get_string(), type_decl->get_len())) {
                        inferred_type = parent_type;
                        parent_type = class_hierarchy_.lookup(inferred_type);
                    }
                    if (inferred_type != NULL && !inferred_type->equal_string(type_decl->get_string(), type_decl->get_len())) {
                        semant_error() << class_->get_filename() << ":" << class_->get_line_number()
                                        << ": Attribute " << attr->get_name()
                                        << " has an incorrect type " << type_decl << "\n";
                        continue;
                    }
                }
            }
        }
    }
}

void ClassTable::check_expr(Expression_class *expr) {
    if (auto *new_ = dynamic_cast<new__class *>(expr); new_ != nullptr) {
        check_new(new_);
    } else if (auto *block = dynamic_cast<block_class *>(expr); block != nullptr) {
        check_block(block);
    } else if (auto *assign = dynamic_cast<assign_class *>(expr); assign != nullptr) {
        check_assign(assign);
    } else if (auto *object = dynamic_cast<object_class *>(expr); object != nullptr) {
        check_object(object);
    } else if (auto *dispatch = dynamic_cast<dispatch_class *>(expr); dispatch != nullptr) {
        check_dispatch(dispatch);
    } else if (auto *int_const = dynamic_cast<int_const_class *>(expr); int_const != nullptr) {
        check_int_const(int_const);
    } else if (auto *bool_const = dynamic_cast<bool_const_class *>(expr); bool_const != nullptr) {
        check_bool_const(bool_const);
    } else if (auto *string_const = dynamic_cast<string_const_class *>(expr); string_const != nullptr) {
        check_string_const(string_const);
    } else if (auto *cond = dynamic_cast<cond_class *>(expr); cond != nullptr) {
        check_cond(cond);
    } else if (auto *let = dynamic_cast<let_class *>(expr); let != nullptr) {
        check_let(let);
    } else if (auto *sub = dynamic_cast<sub_class *>(expr); sub != nullptr) {
        check_sub(sub);
    } else if (auto *loop = dynamic_cast<loop_class *>(expr); loop != nullptr) {
        check_loop(loop);
    } else if (auto *plus = dynamic_cast<plus_class *>(expr); plus != nullptr) {
        check_plus(plus);
    } else if (auto *eq = dynamic_cast<eq_class *>(expr); eq != nullptr) {
        check_eq(eq);
    } else {
        cout << "Unknown expression type: " << typeid(*expr).name() << endl;
    }
}


void ClassTable::check_new(new__class *new_) {
    auto *type_name = new_->get_type_name();
    if (class_tables_.lookup(type_name) == nullptr) {
        semant_error() << "Type name: " << type_name
                       << " is not defined.";
        return;
    }
    new_->set_type(type_name);
}


void ClassTable::check_block(block_class *block) {
    auto *body = block->get_body();    
    for (int i = body->first(); body->more(i); i = body->next(i)) {
        auto *expr = body->nth(i);
        check_expr(expr);
        block->set_type(expr->get_type());
    }
}


void ClassTable::check_assign(assign_class *assign) {
    auto *obj_name = assign->get_name();
    auto *obj_type = symbol_tables_.lookup(current_class_name_)->lookup(obj_name);
    if (obj_type == nullptr) {
        semant_error() << current_file_name_ << ":" << assign->get_line_number()
                        << ": Undeclared identifier " << obj_name
                        << ".\n";
        return;
    }

    auto *expr = assign->get_expr();
    check_expr(expr);
    if (!expr->get_type()->equal_string(obj_type->get_string(), obj_type->get_len())) {
        semant_error() << current_file_name_ << ":" << assign->get_line_number()
                        << ": Type " << expr->get_type()
                        << " of assigned expression does not conform to declared type " << obj_type
                        << " of identifier " << obj_name
                        << ".\n";
        return;
    }
    assign->set_type(obj_type);
}


void ClassTable::check_object(object_class *object) {
    auto *name = object->get_name();
    if (name->equal_string(self->get_string(), self->get_len())) {
        object->set_type(SELF_TYPE);
        return;
    }

    auto *symbol_table = symbol_tables_.lookup(current_class_name_);
    auto *type = symbol_table->lookup(name);
    if (type == nullptr) {
        semant_error() << current_file_name_ << ":" << object->get_line_number()
                << ": Undeclared identifier " << name
                << ".\n";
        return;
    }
    object->set_type(type);
}


void ClassTable::check_dispatch(dispatch_class *dispatch) {
    auto *expr = dispatch->get_expr();
    check_expr(expr);
    auto *inferred_type = expr->get_type();
    auto *method_name = dispatch->get_name();
    auto *actual_args = dispatch->get_actual();
    if (inferred_type == nullptr) {
        semant_error() << current_file_name_ << ":" << dispatch->get_line_number()
                << ": Undeclared identifier " << method_name
                << ".\n";
        return;
    }

    if (inferred_type->equal_string(SELF_TYPE->get_string(), SELF_TYPE->get_len())) {
        inferred_type = current_class_name_;
    }

    auto *feature_table = class_tables_.lookup(inferred_type);
    auto *feature = feature_table->lookup(method_name); 
    auto *parent_type = class_hierarchy_.lookup(inferred_type);
    while (feature == nullptr && parent_type != nullptr) {
        feature_table = class_tables_.lookup(parent_type);
        feature = feature_table->lookup(method_name);
        parent_type = class_hierarchy_.lookup(parent_type);
    }

    if (feature == nullptr) {
        semant_error() << current_file_name_ << ":" << dispatch->get_line_number()
                << ": Dispatch to undefined method " << method_name
                << ".\n";
        return;
    }

    if (auto *method = dynamic_cast<method_class *>(feature); method != nullptr) {
        auto *formals = method->get_formals();
        auto *return_type = method->get_return_type();
        if (formals->len() != actual_args->len()) {
            semant_error() << current_file_name_ << ":" << dispatch->get_line_number()
                    << ": Method " << method_name
                    << " called with wrong number of arguments.\n";
            return;
        }

        for (int i = actual_args->first(); actual_args->more(i); i = actual_args->next(i)) {
            auto *actual_arg = actual_args->nth(i);
            check_expr(actual_arg);
            auto *expected_arg = dynamic_cast<formal_class *>(formals->nth(i));
            auto *expected_type = expected_arg->get_type_decl();
            if (!actual_arg->get_type()->equal_string(expected_type->get_string(), expected_type->get_len())) {
                semant_error() << current_file_name_ << ":" << dispatch->get_line_number()
                    << ": In call of method " << method_name
                    << ", type " << actual_arg->get_type()
                    << " of parameter " << expected_arg->get_name()
                    << " does not conform to declared type " << expected_type
                    << ".\n";
                continue;
            }
        }

        if (return_type->equal_string(SELF_TYPE->get_string(), SELF_TYPE->get_len())
            && !inferred_type->equal_string(current_class_name_->get_string(), current_class_name_->get_len())) {
            dispatch->set_type(symbol_tables_.lookup(inferred_type)->lookup(method_name));
        } else {
            dispatch->set_type(return_type);
        }
    }
}


void ClassTable::check_int_const(int_const_class *int_const) {
    int_const->set_type(Int);
}


void ClassTable::check_bool_const(bool_const_class *bool_const) {
    bool_const->set_type(Bool);
}


void ClassTable::check_string_const(string_const_class *string_const) {
    string_const->set_type(Str);
}


void ClassTable::check_cond(cond_class *cond) {
    check_expr(cond->get_pred());
    auto *then_expr = cond->get_then_exp();
    check_expr(then_expr);
    cond->set_type(then_expr->get_type());
}


void ClassTable::check_let(let_class *let) {
    auto *symbol_table = symbol_tables_.lookup(current_class_name_);
    symbol_table->enterscope();

    auto *id = let->get_identifier();
    auto *type_decl = let->get_type_decl();
    symbol_table->addid(id, type_decl);
    auto *init = let->get_init();
    auto *body = let->get_body();
    check_expr(body);

    symbol_table->exitscope();
}


void ClassTable::check_sub(sub_class *sub) {
    check_expr(sub->get_e1());
    check_expr(sub->get_e2());
}


void ClassTable::check_loop(loop_class *loop) {
    check_expr(loop->get_pred());
    check_expr(loop->get_body());
}


void ClassTable::check_plus(plus_class *plus) {
    check_expr(plus->get_e1());
    check_expr(plus->get_e2());
    plus->set_type(plus->get_e1()->get_type());
}


void ClassTable::check_eq(eq_class *eq) {
    check_expr(eq->get_e1());
    check_expr(eq->get_e2());
    eq->set_type(eq->get_e1()->get_type());
}


/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */

    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}


