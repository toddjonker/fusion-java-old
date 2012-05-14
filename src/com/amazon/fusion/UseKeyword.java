// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonSymbol;
import com.amazon.ion.IonValue;

/**
 *
 */
final class UseKeyword
    extends KeywordValue
{
    private final ModuleNameResolver myModuleNameResolver;

    UseKeyword(ModuleNameResolver moduleNameResolver)
    {
        super("MODULE", "doc");
        myModuleNameResolver = moduleNameResolver;
    }


    @Override
    FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
        throws FusionException
    {
        Namespace namespace = env.namespace();

        ModuleInstance module;

        IonValue modStx = expr.get(1);
        if (modStx instanceof IonSymbol)
        {
            IonSymbol name = (IonSymbol) modStx;

            // TODO error handling
            module = (ModuleInstance) eval.eval(env, name);
            namespace.use(module);
        }
        else
        {
            ModuleIdentity id =
                myModuleNameResolver.resolve(eval, env, modStx);
            namespace.use(id);
        }

        return UNDEF;
    }
}
