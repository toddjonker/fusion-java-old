// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.ModuleIdentity.intern;


/**
 * The baseline bindings for Fusion.
 */
final class BaseModule
    extends ModuleInstance
{
    static final String BASE_NAME = "fusion/base";
    static final ModuleIdentity BASE_IDENTITY = intern(BASE_NAME);

    static final String LAMBDA = "lambda";
    static final String LETREC = "letrec";


    BaseModule(Evaluator eval, Namespace ns)
        throws FusionException
    {
        super(BASE_IDENTITY, ns);
        inferName("fusion_base");

        FusionValue userDir =
            eval.newString(System.getProperty("user.dir"));
        DynamicParameter currentDirectory =
            new DynamicParameter(userDir);
        DynamicParameter currentLoadRelativeDirectory =
            new DynamicParameter(UNDEF);
        DynamicParameter currentModuleDeclareName =
            new DynamicParameter(UNDEF);
        LoadHandler loadHandler =
            new LoadHandler(currentLoadRelativeDirectory, currentDirectory);
        ModuleNameResolver resolver =
            new ModuleNameResolver(loadHandler,
                                   currentLoadRelativeDirectory,
                                   currentDirectory,
                                   currentModuleDeclareName);
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
        ns.bind("empty_stream", new EmptyStreamProc());
        ns.bind("eval_file", evalFile);
        ns.bind("exit", new ExitProc());
        ns.bind("for_each_field", new ForEachFieldProc());
        ns.bind("for_list", new ForListKeyword());
        ns.bind("help", new HelpProc());
        ns.bind("if", new IfKeyword());
        ns.bind("inject_stream", new StreamInjectProc());
        ns.bind("is_null", new IsNullProc());
        ns.bind("is_undef", new IsUndefProc());
        ns.bind(LAMBDA, lambda);
        ns.bind("let", new LetKeyword());
        ns.bind(LETREC, new LetrecKeyword());
        ns.bind("list_bindings", new ListBindingsKeyword());
        ns.bind("make_parameter", new MakeParameterProc());
        ns.bind("module", new ModuleKeyword(currentModuleDeclareName));
        ns.bind("not", new NotProc());
        ns.bind("or", new OrKeyword());
        ns.bind("parameterize", new ParameterizeKeyword());
        ns.bind("quote", new QuoteKeyword());
        ns.bind("read", new ReadProc());
        ns.bind("remove", new RemoveProc());
        ns.bind("size", new SizeProc());
        ns.bind("stream_cross_product", new StreamCrossProductProc());
        ns.bind("stream_cross_apply", new StreamCrossApplyProc());
        ns.bind("stream_for", new IonListToStreamProc());
        ns.bind("stream_has_next", new StreamHasNextProc());
        ns.bind("stream_next", new StreamNextProc());
        ns.bind("stream_project", new StreamProjectProc());
        ns.bind("stream_select",  new StreamSelectProc());
        ns.bind("stream_to_list", new StreamToIonListProc());
        ns.bind("stream_union", new StreamUnionProc());
        ns.bind("undef", FusionValue.UNDEF);
        ns.bind("use", new UseKeyword(resolver));
        ns.bind("write", new WriteProc());
    }
}
