// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


/**
 * The baseline bindings for Fusion.
 */
class BaseModule
    extends ModuleInstance
{
    BaseModule(Evaluator eval)
    {
        super(new Namespace());
        inferName("fusion_base");

        Namespace ns = getNamespace();

        FusionValue userDir =
            eval.newString(System.getProperty("user.dir"));
        DynamicParameter currentDirectory =
            new DynamicParameter(userDir);
        LoadHandler loadHandler =
            new LoadHandler(currentDirectory);
        EvalFileKeyword evalFile =
            new EvalFileKeyword(loadHandler);

        ns.bind("*", new ProductFunction());
        ns.bind("+", new SumFunction());
        ns.bind("-", new DifferenceFunction());
        ns.bind(".", new DotFunction());
        ns.bind("=", new EqualFunction());
        ns.bind("add", new AddFunction());
        ns.bind("and", new AndKeyword());
        ns.bind("assert", new AssertKeyword());
        ns.bind("begin", new BeginKeyword());
        ns.bind("current_directory", currentDirectory);
        ns.bind("define", new DefineKeyword());
        ns.bind("display", new DisplayFunction());
        ns.bind("eval_file", evalFile);
        ns.bind("exit", new ExitFunction());
        ns.bind("for_each_field", new ForEachFieldFunction());
        ns.bind("func", new FuncKeyword());
        ns.bind("help", new HelpFunction());
        ns.bind("if", new IfKeyword());
        ns.bind("is_null", new IsNullFunction());
        ns.bind("is_undef", new IsUndefFunction());
        ns.bind("let", new LetKeyword());
        ns.bind("letrec", new LetrecKeyword());
        ns.bind("list_bindings", new ListBindingsKeyword());
        ns.bind("make_parameter", new MakeParameterFunction());
        ns.bind("module", new ModuleKeyword());
        ns.bind("not", new NotFunction());
        ns.bind("or", new OrKeyword());
        ns.bind("parameterize", new ParameterizeKeyword());
        ns.bind("quote", new QuoteKeyword());
        ns.bind("read", new ReadFunction());
        ns.bind("remove", new RemoveFunction());
        ns.bind("size", new SizeFunction());
        ns.bind("undef", FusionValue.UNDEF);
        ns.bind("use", new UseKeyword(loadHandler));
        ns.bind("write", new WriteFunction());
    }
}
