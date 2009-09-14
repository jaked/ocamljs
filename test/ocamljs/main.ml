open OUnit

let tests = "Ocamljs" >::: [
  Oo_class_arg.tests;
  Oo_class_let.tests;
  Oo_class.tests;
  Oo_cloning.tests;
  Oo_funobj.tests;
  Oo_immediate.tests;
  Oo_inherit.tests;
  Oo_init.tests;
  Oo_private.tests;
  Oo_self.tests;
  Oo_super.tests;
  Oo_this_bug.tests;
  Oo_virtual.tests;
  Raiseargs.tests;
]

;;

OUnit.run_test_tt_main tests
