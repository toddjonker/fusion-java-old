// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
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
        IonValue modStx = requiredForm("module spec", 1, expr);
        Namespace namespace = env.namespace();
        use(eval, namespace, modStx);
        return UNDEF;
    }

    void use(Evaluator eval, Namespace ns, IonValue moduleSpec)
        throws FusionException
    {
        ModuleIdentity id = myModuleNameResolver.resolve(eval, moduleSpec);
        ns.use(id);
    }
}
