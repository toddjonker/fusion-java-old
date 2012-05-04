// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


/**
 * The baseline bindings for Fusion.
 */
class BaseModule
    extends ModuleInstance
{
    static final String LAMBDA = "lambda";
    static final String LETREC = "letrec";


    BaseModule(Evaluator eval)
    {
        super(new Namespace());
        inferName("fusion_base");

        Namespace ns = getNamespace();

        FusionValue userDir =
            eval.newString(System.getProperty("user.dir"));
        DynamicParameter currentDirectory =
            new DynamicParameter(userDir);
        DynamicParameter currentLoadRelativeDirectory =
            new DynamicParameter(UNDEF);
        LoadHandler loadHandler =
            new LoadHandler(currentLoadRelativeDirectory, currentDirectory);
        ModuleNameResolver resolver =
            new ModuleNameResolver(currentLoadRelativeDirectory, currentDirectory);
        EvalFileKeyword evalFile =
            new EvalFileKeyword(loadHandler);
        LambdaKeyword lambda = new LambdaKeyword();

        ns.bind("*", new ProductProc());
        ns.bind("+", new SumProc());
        ns.bind("-", new DifferenceProc());
        ns.bind(".", new DotProc());
        ns.bind("=", new EqualProc());
        ns.bind("add", new AddProc());
        ns.bind("and", new AndKeyword());
        ns.bind("apply", new ApplyProc());
        ns.bind("assert", new AssertKeyword());
        ns.bind("begin", new BeginKeyword());
        ns.bind("current_directory", currentDirectory);
        ns.bind("define", new DefineKeyword());
        ns.bind("display", new DisplayProc());
        ns.bind("eval_file", evalFile);
        ns.bind("exit", new ExitProc());
        ns.bind("for_each_field", new ForEachFieldProc());
        ns.bind("help", new HelpProc());
        ns.bind("if", new IfKeyword());
        ns.bind("is_null", new IsNullProc());
        ns.bind("is_undef", new IsUndefProc());
        ns.bind(LAMBDA, lambda);
        ns.bind("let", new LetKeyword());
        ns.bind(LETREC, new LetrecKeyword());
        ns.bind("list_bindings", new ListBindingsKeyword());
        ns.bind("make_parameter", new MakeParameterProc());
        ns.bind("module", new ModuleKeyword());
        ns.bind("not", new NotProc());
        ns.bind("or", new OrKeyword());
        ns.bind("parameterize", new ParameterizeKeyword());
        ns.bind("quote", new QuoteKeyword());
        ns.bind("read", new ReadProc());
        ns.bind("remove", new RemoveProc());
        ns.bind("size", new SizeProc());
        ns.bind("undef", FusionValue.UNDEF);
        ns.bind("use", new UseKeyword(resolver, loadHandler));
        ns.bind("write", new WriteProc());
    }
}
