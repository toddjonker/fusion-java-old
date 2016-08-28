// Copyright (c) 2012-2016 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionSexp.isPair;
import static com.amazon.fusion.FusionSexp.isSexp;
import static com.amazon.fusion.FusionSexp.unsafePairHead;
import static com.amazon.fusion.FusionSexp.unsafePairTail;
import static com.amazon.fusion.FusionSymbol.makeSymbol;
import static com.amazon.fusion.FusionText.isText;
import static com.amazon.fusion.FusionVoid.voidValue;
import static com.amazon.fusion.GlobalState.REQUIRE;
import static com.amazon.ion.util.IonTextUtils.printString;
import com.amazon.fusion.Namespace.RequireRenameMapping;
import java.util.ArrayList;
import java.util.Arrays;


/**
 * Note that there is a resiliency problem with respect to multiple
 * imports: if a library module adds a binding thats already used by a user
 * module, there can be a conflict introduced by a new release of the library.
 * The same problem exists with Java code using {@code import *}, and the
 * recommended preventative measure is the same: robust modules should declare
 * their imported names explicitly using {@code only_in}, rather than using the
 * default "import everything" behavior.
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
              "Each `require_clause` denotes some bindings to be imported. The following clause\n" +
              "forms are allowed:\n" +
              "\n" +
              "  * A string or symbol containing a [module path][]; all names `provide`d by\n" +
              "    the referenced module are imported.\n" +
              "  * [`only_in`][only_in] enumerates a set of names to import.\n" +
              "\n" +
              "Within a module, `require` declarations are processed before other forms,\n" +
              "regardless of their order within the module source, and imported bindings are\n" +
              "scoped across the entire module. No identifier may be imported multiple times,\n" +
              "unless all such bindings refer to the same originating definition. Furthermore,\n" +
              "no identifier may have both an import and a module-level definition.\n" +
              "In other words: module-level bindings introduced by `require` or `define` must\n" +
              "not conflict, although either may shadow same-named bindings introduced by the\n" +
              "module's language declaration.\n" +
              "\n" +
              "At top level, `require` will replace an existing import, and may shadow an\n" +
              "existing top-level definition.\n" +
              "\n" +
              "[module path]: module.html#ref\n" +
              "[only_in]:     fusion/module.html#only_in\n");
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
                expandRequireSexp(expander, eval, check,
                                  (SyntaxSexp) spec, expanded);
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

    private void expandRequireSexp(Expander expander,
                                   Evaluator eval,
                                   SyntaxChecker requireCheck,
                                   SyntaxSexp spec,
                                   ArrayList<SyntaxValue> expandedSpecs)
        throws FusionException
    {
        SyntaxChecker check = new SyntaxChecker(eval, spec);

        GlobalState globalState = eval.getGlobalState();

        Binding b = spec.firstTargetBinding(eval);
        if (b == globalState.myKernelOnlyInBinding)
        {
            int arity = check.arityAtLeast(2);

            SyntaxValue[] children = spec.extract(eval);
            children[0] = SyntaxSymbol.make(eval, children[0].getLocation(),
                                            makeSymbol(eval, "only"));

            // TODO This should visit the module (run its phase-1 code)
            //      but not instantiate it.
            String path = check.requiredText(eval, "module path", 1);
            checkValidModulePath(eval, check, path);

            for (int i = 2; i < arity; i++)
            {
                // "The lexical context of the raw-require-spec determines the
                // context of introduced identifiers.  The exception is the
                // rename sub-form, where the lexical context of the local-id
                // is preserved."
                // https://docs.racket-lang.org/reference/require.html
                //
                // But: Racket expands only_in to rename!

                check.requiredIdentifier(i);
            }

            expandedSpecs.add(SyntaxSexp.make(eval, children));
        }
        else
        {
            throw requireCheck.failure("invalid require-spec");
        }
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
        SyntaxSymbol requireSym = (SyntaxSymbol) stx.get(eval, 0);

        CompiledRequireSpec[] compiledSpecs =
            new CompiledRequireSpec[arity - 1];

        int i = 0;
        for (Object p = unsafePairTail(eval, stx.unwrap(eval));
             isPair(eval, p);
             p = unsafePairTail(eval, p), i++)
        {
            SyntaxValue spec = (SyntaxValue) unsafePairHead(eval, p);

            try
            {
                compiledSpecs[i] =
                    compileSpec(eval, baseModule, check, requireSym, spec);
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
                                            SyntaxSymbol lexicalContext,
                                            SyntaxValue spec)
        throws FusionException
    {
        if (spec instanceof SyntaxSexp)
        {
            SyntaxSexp sexp = (SyntaxSexp) spec;
            switch (sexp.firstIdentifier(eval).getName().stringValue())
            {
                case "only":
                {
                    ModuleIdentity moduleId =
                        myModuleNameResolver.resolve(eval, baseModule,
                                                     sexp.get(eval, 1), true);

                    int idCount = sexp.size() - 2;
                    RequireRenameMapping[] mappings =
                        new RequireRenameMapping[idCount];
                    for (int i = 0; i < idCount; i++)
                    {
                        SyntaxSymbol id = (SyntaxSymbol) sexp.get(eval, i + 2);
                        mappings[i] =
                            new RequireRenameMapping(id, id.getName());
                    }

                    return new CompiledPartialRequire(moduleId, mappings);
                }
                default:
                {
                    throw requireCheck.failure("invalid provide-spec");
                }
            }
        }
        else // It's a (string or symbol) module path.
        {
            ModuleIdentity moduleId =
                myModuleNameResolver.resolve(eval, baseModule, spec, true);
            return new CompiledFullRequire(moduleId, lexicalContext);
        }
    }


    /**
     * This class primarily provides syntax to bind to require-clauses so that
     * {@link RequireForm} can compare bindings, not symbolic names.  This is
     * as specified by Racket.
     */
    private abstract static class AbstractRequireClauseForm
        extends SyntacticForm
    {
        AbstractRequireClauseForm(String bodyPattern, String doc)
        {
            super(bodyPattern, doc);
        }

        @Override
        SyntaxValue expand(Expander expander, Environment env, SyntaxSexp stx)
            throws FusionException
        {
            throw check(expander, stx).failure("must be used inside `require`");
        }

        @Override
        CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp stx)
            throws FusionException
        {
            throw new IllegalStateException("Shouldn't be compiled");
        }
    }


    static final class OnlyInForm
        extends AbstractRequireClauseForm
    {
        OnlyInForm()
        {
            //    "                                                                               |
            super("module_path id ...",
                  "A `require` clause that imports only the given `id`s from a module.\n" +
                  "If an `id` is not provided by the module, a syntax error is reported.\n" +
                  "\n" +
                  "This form can only appear within `require`.");
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
        private final SyntaxSymbol myLexicalContext;

        private CompiledFullRequire(ModuleIdentity usedModuleId,
                                    SyntaxSymbol lexicalContext)
        {
            super(usedModuleId);
            myLexicalContext = lexicalContext;
        }

        @Override
        public void eval(Evaluator eval, Namespace namespace)
            throws FusionException
        {
            namespace.require(eval, myLexicalContext, myUsedModuleId);
        }
    }


    private static final class CompiledPartialRequire
        extends CompiledRequireSpec
    {
        private final RequireRenameMapping[] myMappings;

        private CompiledPartialRequire(ModuleIdentity usedModuleId,
                                       RequireRenameMapping[] mappings)
        {
            super(usedModuleId);
            myMappings = mappings;
        }

        @Override
        public void eval(Evaluator eval, Namespace namespace)
            throws FusionException
        {
            namespace.require(eval, myUsedModuleId,
                              Arrays.asList(myMappings).iterator());
        }
    }
}
