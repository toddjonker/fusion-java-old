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
    SyntaxValue expand(Evaluator eval, Environment env, SyntaxSexp source)
        throws SyntaxFailure
    {
        requiredForm("module spec", 1, source);
        return source;
    }


    @Override
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp expr)
        throws FusionException
    {
        // TODO Should we resolve the name at compile-time?
        SyntaxValue moduleSpec = expr.get(1);
        ModuleIdentity id = myModuleNameResolver.resolve(eval, moduleSpec);
        return new CompiledUse(id);
    }


    void use(Evaluator eval, Namespace ns, SyntaxValue moduleSpec)
        throws FusionException
    {
        ModuleIdentity id = myModuleNameResolver.resolve(eval, moduleSpec);
        ns.use(id);
    }


    //========================================================================


    private static final class CompiledUse
        implements CompiledForm
    {
        private final ModuleIdentity myUsedModuleId;

        public CompiledUse(ModuleIdentity usedModuleId)
        {
            myUsedModuleId = usedModuleId;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            Namespace namespace = (Namespace) store.runtimeNamespace();
            namespace.use(myUsedModuleId);
            return UNDEF;
        }
    }
}
