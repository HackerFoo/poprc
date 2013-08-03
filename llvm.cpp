#include <llvm/Module.h>
#include <llvm/Function.h>
#include <llvm/PassManager.h>
#include <llvm/CallingConv.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/Assembly/PrintModulePass.h>
#include <llvm/IRBuilder.h>
#include <llvm/Support/raw_ostream.h>
#include "llvm.h"

using namespace llvm;

void printModule(Module *mod) {
  verifyModule(*mod, PrintMessageAction);
  PassManager PM;
  PM.add(createPrintModulePass(&outs()));
  PM.run(*mod);
}

Module *makeModule(cell_t *p) {
  LLVMContext &ctx = getGlobalContext();
  Module *mod = new Module("test", ctx);
  Constant *c = mod->getOrInsertFunction("add3",
		     /* return type */	 IntegerType::get(ctx, 64),
		     /* arg types   */   IntegerType::get(ctx, 64),
					 IntegerType::get(ctx, 64),
					 IntegerType::get(ctx, 64),
					 NULL);
  Function *add3 = cast<Function>(c);
  add3->setCallingConv(CallingConv::C);

  Value *x, *y, *z;
  Function::arg_iterator args = add3->arg_begin();
  (x = args++)->setName("x");
  (y = args++)->setName("y");
  (z = args++)->setName("z");

  BasicBlock *block = BasicBlock::Create(getGlobalContext(), "entry", add3);
  IRBuilder<> builder(block);

  Value *u, *v;
  u = builder.CreateBinOp(Instruction::Add, x, y, "u");
  v = builder.CreateBinOp(Instruction::Add, u, z, "v");
  builder.CreateRet(v);
  return mod;
}

void print_llvm_ir(cell_t *c) {
  Module *m = makeModule(c);
  printModule(m);
  free(m);
}
