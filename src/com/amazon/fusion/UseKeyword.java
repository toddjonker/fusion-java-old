// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonString;
import com.amazon.ion.IonSymbol;
import com.amazon.ion.IonValue;
import java.io.File;

/**
 *
 */
final class UseKeyword
    extends KeywordValue
{
    private final ModuleNameResolver myModuleNameResolver;
    private final LoadHandler myLoadHandler;

    UseKeyword(ModuleNameResolver moduleNameResolver, LoadHandler loadHandler)
    {
        super("MODULE", "doc");
        myModuleNameResolver = moduleNameResolver;
        myLoadHandler = loadHandler;
    }


    @Override
    FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
        throws FusionException
    {
        Namespace namespace = env.rootNamespace();

        ModuleInstance module;

        IonValue modStx = expr.get(1);
        if (modStx instanceof IonSymbol)
        {
            IonSymbol name = (IonSymbol) modStx;

            module = (ModuleInstance) eval.eval(env, name);
        }
        else
        {
            String path = ((IonString) modStx).stringValue();
            File file = myModuleNameResolver.resolve(eval, path);
            module = myLoadHandler.loadModule(eval, file);
        }

        namespace.use(module);

        return UNDEF;
    }
}
