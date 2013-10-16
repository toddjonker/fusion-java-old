// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionVoid.voidValue;

final class UseForm
    extends SyntacticForm
{
    private final ModuleNameResolver myModuleNameResolver;

    UseForm(ModuleNameResolver moduleNameResolver)
    {
        super("MODULE", "doc");
        myModuleNameResolver = moduleNameResolver;
    }


    @Override
    SyntaxValue expand(Expander expander, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        check(expander, stx).requiredForm("module spec", 1);
        return stx;
    }


    @Override
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        // TODO Should we resolve the name at compile-time?
        SyntaxValue moduleSpec = stx.get(eval, 1);
        ModuleIdentity id = myModuleNameResolver.resolve(eval, moduleSpec);
        return new CompiledUse(id);
    }


    void use(Evaluator eval, Namespace ns, SyntaxValue moduleSpec)
        throws FusionException
    {
        ModuleIdentity id = myModuleNameResolver.resolve(eval, moduleSpec);
        ns.require(eval, id);
    }


    //========================================================================


    private static final class CompiledUse
        implements CompiledForm
    {
        private final ModuleIdentity myUsedModuleId;

        private CompiledUse(ModuleIdentity usedModuleId)
        {
            myUsedModuleId = usedModuleId;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            // These forms are evaluated during expansion of a module.
            // So it's never entered with a "real" Store.
            Namespace namespace = (Namespace) store.namespace();
            namespace.require(eval, myUsedModuleId);
            return voidValue(eval);
        }
    }
}
