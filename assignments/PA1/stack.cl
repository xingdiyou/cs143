(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)

class ListNode {
   cmd_ : StackCommand;
   next_ : ListNode;

   init(cmd : StackCommand, next : ListNode) : SELF_TYPE {
      {
         cmd_ <- cmd;
         next_ <- next;
         self;
      }
   };

   get_cmd() : StackCommand {
      cmd_
   };

   set_cmd(cmd : StackCommand) : Object {
      cmd_ <- cmd
   };

   get_next() : ListNode {
      next_
   };

   set_next(next : ListNode) : Object {
      next_ <- next
   };
};

class Stack {
   dummy_head_ : ListNode <- new ListNode;
   size_ : Int <- 0;

   push(cmd : StackCommand) : Object {
      {
         dummy_head_.set_next((new ListNode).init(cmd, dummy_head_.get_next()));
         size_ <- size_ + 1;
      }
   };

   pop() : StackCommand {
      let next : ListNode <- dummy_head_.get_next() in {
         dummy_head_.set_next(next.get_next());
         size_ <- size_ - 1;
         next.get_cmd();
      }
   };

   is_empty() : Bool {
      isvoid dummy_head_.get_next()
   };
};

class StackCommand inherits IO {
   value_ : String;

   init(value : String) : SELF_TYPE {
      {
         value_ <- value;
         self;
      }
   };

   execute(stack : Stack) : Object {
      {
         out_string("Not implemented\n");
         abort();
      }
   };

   get_value() : String {
      value_
   };
};

class PlusCommand inherits StackCommand {
   execute(stack : Stack) : Object {
      if not stack.is_empty() then
         let a : StackCommand <- stack.pop() in
            if not stack.is_empty() then 
               let b : StackCommand <- stack.pop() in
                  let a2i : A2I <- new A2I in
                     stack.push((new IntCommand).init(a2i.i2a(a2i.a2i(a.get_value()) + a2i.a2i(b.get_value()))))
            else
               stack.push(a)
            fi
      else
         self
      fi
   };
};

class SwapCommand inherits StackCommand {
   execute(stack : Stack) : Object {
      if not stack.is_empty() then
         let a : StackCommand <- stack.pop() in
            if not stack.is_empty() then
               let b : StackCommand <- stack.pop() in {
                  stack.push(a);
                  stack.push(b);
               }
            else
               stack.push(a)
            fi
      else
         self
      fi
   };
};

class IntCommand inherits StackCommand {
   execute(stack : Stack) : Object {
      stack.push((new IntCommand).init(get_value()))
   };
};

class CommandFactory {
   create_command(cmd : String) : StackCommand {
      if cmd = "+" then
         (new PlusCommand).init(cmd)
      else if cmd = "s" then
         (new SwapCommand).init(cmd)
      else
         (new IntCommand).init(cmd)
      fi fi
   };
};

class StackMachine inherits IO {
   stack_ : Stack <- new Stack;

   evaluate() : Object {
      if not stack_.is_empty() then
         let cmd : StackCommand <- stack_.pop() in
            cmd.execute(stack_)
      else
         self
      fi
   };

   store(cmd : StackCommand) : Object {
      stack_.push(cmd)
   };

   display() : Object {
      if not stack_.is_empty() then
         let command : StackCommand <- stack_.pop() in
            let io : IO <- new IO in {
               io.out_string(command.get_value());
               io.out_string("\n");
               display();      
               stack_.push(command);
            }
      else
         self
      fi
   };
};

class Main inherits IO {
   machine_ : StackMachine <- new StackMachine;
   factory_ : CommandFactory <- new CommandFactory;

   main() : Object {
      let running : Bool <- true in
         while running loop {
            out_string(">");
            let cmd : String <- in_string() in
               if cmd = "x" then
                  running <- false
               else if cmd = "e" then
                  machine_.evaluate()
               else if cmd = "d" then
                  machine_.display()
               else
                  machine_.store(factory_.create_command(cmd))
               fi fi fi;
         } pool
   };
};
