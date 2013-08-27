// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionVoid.voidValue;

final class RequireForm
    extends SyntacticForm
{
    private final ModuleNameResolver myModuleNameResolver;

    RequireForm(ModuleNameResolver moduleNameResolver)
    {
        //    "                                                                               |
        super("require_clause ...+",
              "Declares bindings to be imported into the enclosing module.  This form may only\n" +
              "appear at module level.\n" +
              "The clauses denote the bindings to be imported.  At present the only clause\n" +
              "form is a string or symbol containing a [module path][]; all bindings\n" +
              "`provide`d by the referenced module are imported." +
              "\n" +
              "[module path]: module.html#ref");
        myModuleNameResolver = moduleNameResolver;
    }


    @Override
    SyntaxValue expand(Expander expander, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        final Evaluator eval = expander.getEvaluator();

        SyntaxChecker check = check(eval, stx);

        if (!expander.isModuleContext() && !expander.isTopLevelContext())
        {
            throw check.failure("May only appear in a top-level context " +
                                "or a module context");
        }

        int arity = check.arityAtLeast(2);

        for (int i = 1; i < arity; i++)
        {
            String path = check.requiredText("module path", i);
            if (! ModuleIdentity.isValidModulePath(path))
            {
                String message =
                    "not a valid module path: " + stx.get(eval, i);
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
            SyntaxValue moduleSpec = stx.get(eval, i);

            ids[i-1] = myModuleNameResolver.resolve(eval, baseModule,
                                                    moduleSpec, true);
        }

        return new CompiledRequire(ids);
    }


    /**
     * @param modulePath is an absolute or relative module path.
     */
    void require(Evaluator eval, Namespace ns, String modulePath)
        throws FusionException
    {
        ModuleIdentity baseModule = ns.getModuleId();
        ModuleIdentity id =
            myModuleNameResolver.resolveModulePath(eval, baseModule,
                                                   modulePath,
                                                   true /* load */,
                                                   null /* stxForErrors */);
        ns.require(id);
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
                namespace.require(id);
            }
            return voidValue(eval);
        }
    }
}
