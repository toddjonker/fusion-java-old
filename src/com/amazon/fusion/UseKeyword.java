// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

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
    SyntaxValue prepare(Evaluator eval, Environment env, SyntaxSexp source)
        throws SyntaxFailure
    {
        SyntaxValue modStx = requiredForm("module spec", 1, source);
        Namespace namespace = env.namespace();
        try
        {
            use(eval, namespace, modStx);
        }
        catch (FusionException e)
        {
            // TODO SyntaxFailure should hold its nested cause.
            String message = "Error loading module:" + e.getMessage();
            throw new SyntaxFailure(getInferredName(), message, source);
        }
        return source;
    }


    @Override
    FusionValue invoke(Evaluator eval, Environment env, SyntaxSexp expr)
        throws FusionException
    {
        // TODO this shouldn't be needed
        SyntaxValue modStx = requiredForm("module spec", 1, expr);
        Namespace namespace = env.namespace();
        use(eval, namespace, modStx);
        return UNDEF;
    }

    void use(Evaluator eval, Namespace ns, SyntaxValue moduleSpec)
        throws FusionException
    {
        ModuleIdentity id = myModuleNameResolver.resolve(eval, moduleSpec);
        ns.use(id);
    }
}
