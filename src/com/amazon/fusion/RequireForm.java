// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionVoid.voidValue;

final class RequireForm
    extends SyntacticForm
{
    private final ModuleNameResolver myModuleNameResolver;

    RequireForm(ModuleNameResolver moduleNameResolver)
    {
        super("MODULE", "doc");
        myModuleNameResolver = moduleNameResolver;
    }


    @Override
    SyntaxValue expand(Expander expander, Environment env, SyntaxSexp stx)
        throws SyntaxFailure
    {
        SyntaxChecker check = check(stx);

        // TODO check expansion context is top-level or module-level

        // TODO what about zero args?
        int arity = check.arityAtLeast(1);

        for (int i = 1; i < arity; i++)
        {
            String path = check.requiredText("module path", i);
            if (! ModuleIdentity.isValidModulePath(path))
            {
                String message = "not a valid module path: " + stx.get(i);
                throw check.failure(message);
            }
        }

        return stx;
    }


    @Override
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        // TODO Should we resolve the name at compile-time or run-time?

        ModuleIdentity baseModule = env.namespace().getModuleId();
        int arity = stx.size();
        ModuleIdentity[] ids = new ModuleIdentity[arity - 1];

        for (int i = 1; i < arity; i++)
        {
            SyntaxValue moduleSpec = stx.get(i);

            ids[i-1] = myModuleNameResolver.resolve(eval, moduleSpec,
                                                    baseModule, true);
        }

        return new CompiledRequire(ids);
    }


    void use(Evaluator eval, Namespace ns, SyntaxValue moduleSpec)
        throws FusionException
    {
        ModuleIdentity id = myModuleNameResolver.resolve(eval, moduleSpec);
        ns.use(id);
    }


    //========================================================================


    private static final class CompiledRequire
        implements CompiledForm
    {
        private final ModuleIdentity[] myUsedModuleIds;

        private CompiledRequire(ModuleIdentity[] usedModuleIds)
        {
            myUsedModuleIds = usedModuleIds;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            // These forms are evaluated during expansion of a module.
            // So it's never entered with a "real" Store.
            Namespace namespace = (Namespace) store.namespace();
            for (ModuleIdentity id : myUsedModuleIds)
            {
                namespace.use(id);
            }
            return voidValue(eval);
        }
    }
}
