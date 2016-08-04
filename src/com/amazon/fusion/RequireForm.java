// Copyright (c) 2012-2016 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionSexp.isSexp;
import static com.amazon.fusion.FusionText.isText;
import static com.amazon.fusion.FusionVoid.voidValue;
import static com.amazon.fusion.GlobalState.REQUIRE;
import static com.amazon.ion.util.IonTextUtils.printString;
import java.util.ArrayList;


/**
 * A note on the design for {@code require}: unlike Racket, Fusion allows
 * imported names to shadow those declared by a module's language. The intent
 * of this change is to make user's source code more resilient to additions
 * to the language.  In particular, this allows a user-owned module to provide
 * an alternate binding for a language feature, and then import that module
 * into many others.
 * <p>
 * Note that there is still a resiliency problem with respect to multiple
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

        ArrayList<SyntaxValue> expanded = new ArrayList<>(stx.size());
        expanded.add(stx.get(eval, 0));

        for (int i = 1; i < arity; i++)
        {
            SyntaxValue spec = stx.get(eval, i);
            Object datum = spec.unwrap(eval);
            if (isText(eval, datum))
            {
                expandRequireModule(eval, check, spec, datum, expanded);
            }
            else if (isSexp(eval, datum))
            {
                expandRequireSexp(eval, check, (SyntaxSexp) spec, expanded);
            }
            else
            {
                throw check.failure("invalid require-spec", spec);
            }
        }

        SyntaxValue[] children = expanded.toArray(SyntaxValue.EMPTY_ARRAY);
        return stx.copyReplacingChildren(eval, children);
    }

    private void expandRequireModule(Evaluator eval,
                                     SyntaxChecker requireCheck,
                                     SyntaxValue spec,
                                     Object textDatum,
                                     ArrayList<SyntaxValue> expandedSpecs)
        throws FusionException
    {
        String path = FusionText.textToJavaString(eval, textDatum);
        checkValidModulePath(eval, requireCheck, path);

        expandedSpecs.add(spec);
    }

    private void expandRequireSexp(Evaluator eval,
                                   SyntaxChecker requireCheck,
                                   SyntaxSexp spec,
                                   ArrayList<SyntaxValue> expandedSpecs)
        throws FusionException
    {
        SyntaxChecker check = new SyntaxChecker(eval, spec);

        throw check.failure("invalid require-spec");
    }

    private void checkValidModulePath(Evaluator     eval,
                                      SyntaxChecker check,
                                      String        path)
        throws SyntaxException
    {
        if (! ModuleIdentity.isValidModulePath(path))
        {
            String message = "invalid module path: " + printString(path);
            throw check.failure(message);
        }
    }


    //========================================================================


    @Override
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        // We resolve the name at compile-time, noting that for `require`
        // the form is immediately evaluated. I don't want to think about what
        // would happen if resolving at runtime gave a different result.

        ModuleIdentity baseModule = env.namespace().getModuleId();
        int arity = stx.size();

        SyntaxChecker check = new SyntaxChecker(eval, REQUIRE, stx);

        CompiledRequireSpec[] compiledSpecs =
            new CompiledRequireSpec[arity - 1];

        for (int i = 1; i < arity; i++)
        {
            SyntaxValue spec = stx.get(eval, i);

            try
            {
                compiledSpecs[i - 1] =
                    compileSpec(eval, baseModule, check, spec);
            }
            catch (FusionException e)
            {
                e.addContext(spec);
                throw e;
            }
        }

        return new CompiledRequire(compiledSpecs);
    }

    private CompiledRequireSpec compileSpec(Evaluator eval,
                                            ModuleIdentity baseModule,
                                            SyntaxChecker requireCheck,
                                            SyntaxValue spec)
        throws FusionException
    {
        if (spec instanceof SyntaxSexp)
        {
            throw requireCheck.failure("invalid require-spec");
        }
        else // It's a (string or symbol) module path.
        {
            ModuleIdentity moduleId =
                myModuleNameResolver.resolve(eval, baseModule, spec, true);
            return new CompiledFullRequire(moduleId);
        }
    }

    //========================================================================


    private static final class CompiledRequire
        implements CompiledForm
    {
        private final CompiledRequireSpec[] mySpecs;

        private CompiledRequire(CompiledRequireSpec[] specs)
        {
            mySpecs = specs;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            // These forms are evaluated during expansion of a module.
            // So it's never entered with a "real" Store.
            Namespace namespace = (Namespace) store.namespace();

            for (CompiledRequireSpec spec : mySpecs)
            {
                spec.eval(eval, namespace);
            }
            return voidValue(eval);
        }
    }

    private abstract static class CompiledRequireSpec
    {
        protected final ModuleIdentity myUsedModuleId;

        private CompiledRequireSpec(ModuleIdentity usedModuleId)
        {
            myUsedModuleId = usedModuleId;
        }

        abstract void eval(Evaluator eval, Namespace namespace)
            throws FusionException;
    }


    private static final class CompiledFullRequire
        extends CompiledRequireSpec
    {
        private CompiledFullRequire(ModuleIdentity usedModuleId)
        {
            super(usedModuleId);
        }

        @Override
        public void eval(Evaluator eval, Namespace namespace)
            throws FusionException
        {
            namespace.require(eval, myUsedModuleId);
        }
    }
}
