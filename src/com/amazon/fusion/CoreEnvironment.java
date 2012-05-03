// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonSymbol;
import com.amazon.ion.IonValue;

/**
 * The core, built-in bindings for Fusion.
 * This is kind-of hacky and will probably be refactored significantly.
 */
class CoreEnvironment
    extends Namespace
    implements Environment
{
    private final class DefineKeyword
        extends KeywordValue
    {
        private DefineKeyword()
        {
            //    "                                                                               |
            super("VAR VALUE",
                  "Defines a global variable VAR with the given VALUE.");
        }

        @Override
        FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
            throws FusionException
        {
            IonSymbol name = (IonSymbol) expr.get(1);
            IonValue ionValue = expr.get(2);

            FusionValue fusionValue = eval.eval(env, ionValue);
            bind(name.stringValue(), fusionValue);

            return fusionValue;
        }
    }


    CoreEnvironment(Evaluator eval)
    {
        FusionValue userDir =
            eval.newString(System.getProperty("user.dir"));
        DynamicParameter currentDirectory =
            new DynamicParameter(userDir);
        LoadHandler loadHandler =
            new LoadHandler(currentDirectory);
        EvalFileKeyword evalFile =
            new EvalFileKeyword(loadHandler);

        bind("*", new ProductFunction());
        bind("+", new SumFunction());
        bind("-", new DifferenceFunction());
        bind(".", new DotFunction());
        bind("=", new EqualFunction());
        bind("add", new AddFunction());
        bind("and", new AndKeyword());
        bind("assert", new AssertKeyword());
        bind("begin", new BeginKeyword());
        bind("current_directory", currentDirectory);
        bind("define", new DefineKeyword());
        bind("display", new DisplayFunction());
        bind("eval_file", evalFile);
        bind("exit", new ExitFunction());
        bind("for_each_field", new ForEachFieldFunction());
        bind("func", new FuncKeyword());
        bind("help", new HelpFunction());
        bind("if", new IfKeyword());
        bind("is_null", new IsNullFunction());
        bind("is_undef", new IsUndefFunction());
        bind("let", new LetKeyword());
        bind("letrec", new LetrecKeyword());
        bind("list_bindings", new ListBindingsKeyword());
        bind("make_parameter", new MakeParameterFunction());
        bind("module", new ModuleKeyword());
        bind("not", new NotFunction());
        bind("or", new OrKeyword());
        bind("parameterize", new ParameterizeKeyword());
        bind("quote", new QuoteKeyword());
        bind("read", new ReadFunction());
        bind("remove", new RemoveFunction());
        bind("size", new SizeFunction());
        bind("undef", FusionValue.UNDEF);
        bind("use", new UseKeyword(loadHandler));
        bind("write", new WriteFunction());
    }
}
