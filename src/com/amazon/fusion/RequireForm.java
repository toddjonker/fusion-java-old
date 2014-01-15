// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionVoid.voidValue;


/**
 * A note on the design for {@code require}: unlike Racket, Fusion allows
 * imported names to shadow those declared by a module's language. The intent
 * of this change is to make user's source code more resiliant to additions
 * to the language.  In particular, this allows a user-owned module to proved
 * an alternate binding for a language feature, and then import that module
 * into many others.
 * <p>
 * Note that there is still a resiliancy problem with respect to multiple
 * imports: if a library module adds a binding thats already used by a user
 * module, there can be a conflict introduced by a new release of the library.
 * The same problem exists with Java code using {@code import *}, and the
 * recommended preventative measure is the same: robust modules should declare
 * their imported names explicitly, rather than using the default "import
 * everything behavior".  (Unfortunately, explicit imports are not yet
 * implemented in Fusion; see FUSION-63).
 */
final class RequireForm
    extends SyntacticForm
{
    private final ModuleNameResolver myModuleNameResolver;

    RequireForm(ModuleNameResolver moduleNameResolver)
    {
        //    "                                                                               |
        super("require_clause ...+",
              "Declares bindings to be imported into the enclosing namespace. This form may\n" +
              "only appear at module level or top level.\n" +
              "\n" +
              "The `require_clause`s denote the bindings to be imported. At present the only\n" +
              "kind of clause allowed is a string or symbol containing a [module path][]; all\n" +
              "bindings `provide`d by the referenced module are imported.\n" +
              "\n" +
              "Within a module, `require` declarations are processed before other forms,\n" +
              "regardless of their order within the module source, and imported bindings are\n" +
              "scoped across the entire module. No identifier may be imported multiple times,\n" +
              "unless all such bindings refer to the same originating declaration. Furthermore,\n" +
              "no identifier may have both an import and a module-level definition.\n" +
              "\n" +
              "In other words: module-level bindings introduced by `require` or `define` must\n" +
              "not conflict, although either may shadow same-named bindings introduced by the\n" +
              "module's language declaration.\n" +
              "\n" +
              "At top level, a single `require` declaration may not introduce the same name\n" +
              "twice with different bindings, but successive `require`s may shadow any existing\n" +
              "top-level binding, regardless of whether it was introduced by a previous\n" +
              "`require` or `define`.\n" +
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
        // We resolve the name at compile-time, noting that for `require`
        // the form is immediately evaluated. I don't want to think about what
        // would happen if resolving at runtime gave a different result.

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
                namespace.require(eval, id);
            }
            return voidValue(eval);
        }
    }
}
