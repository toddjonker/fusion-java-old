// Copyright (c) 2012-2016 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionSexp.isSexp;
import static com.amazon.fusion.FusionSymbol.isSymbol;
import static com.amazon.fusion.FusionSymbol.makeSymbol;
import static com.amazon.fusion.Syntax.datumToSyntax;
import static com.amazon.ion.util.IonTextUtils.printQuotedSymbol;
import com.amazon.fusion.FusionSymbol.BaseSymbol;
import com.amazon.fusion.Namespace.NsDefinedBinding;
import java.util.ArrayList;


final class ProvideForm
    extends SyntacticForm
{
    ProvideForm()
    {
        //    "                                                                               |
        super("provide_clause ...",
              "Declares bindings to be exported from the enclosing module.  This form may only\n" +
              "appear at module level.\n" +
              "\n" +
              "Each `provide_clause` denotes some names to be exported. The following clause\n" +
              "forms are allowed:\n" +
              "\n" +
              "  * An identifier defined at module-level or imported from another module.\n" +
              "  * [`all_defined_out`][all_defined_out] exports all module-level definitions.\n" +
              "  * [`rename_out`][rename_out] exports selected bindings, giving them new names\n" +
              "    on the way out.\n" +
              "\n" +
              "Within a module, a single `provide` form with multiple clauses behaves\n" +
              " identically to multiple `provide` forms with single clauses.\n" +
              "\n" +
              "[all_defined_out]: fusion/module.html#all_defined_out\n" +
              "[rename_out]:      fusion/module.html#rename_out\n");
    }


    /**
     * Note that we expand `provide` after expanding the rest of the module.
     */
    @Override
    SyntaxValue expand(Expander expander, Environment env, SyntaxSexp form)
        throws FusionException
    {
        final Evaluator eval = expander.getEvaluator();

        SyntaxChecker check = check(eval, form);

        if (!expander.isModuleContext())
        {
            throw check.failure("May only appear in a module context");
        }


        Namespace moduleNamespace = (Namespace) env;

        ArrayList<SyntaxValue> expanded = new ArrayList<>(form.size());
        expanded.add(form.get(eval, 0));

        for (int i = 1; i < form.size(); i++)
        {
            SyntaxValue spec = form.get(eval, i);
            Object datum = spec.unwrap(eval);
            if (isSymbol(eval, datum))
            {
                check.requiredIdentifier("bound identifier", i);

                expandProvideId(moduleNamespace, (SyntaxSymbol) spec,
                                check, expanded);
            }
            else if (isSexp(eval, datum))
            {
                expandProvideSexp(eval, moduleNamespace,
                                  (SyntaxSexp) spec,
                                  check,
                                  expanded);
            }
            else
            {
                throw check.failure("expected provide-spec", spec);
            }
        }

        SyntaxValue[] children = expanded.toArray(SyntaxValue.EMPTY_ARRAY);
        return form.copyReplacingChildren(eval, children);
    }

    private void expandProvideId(Namespace moduleNamespace,
                                 SyntaxSymbol identifier,
                                 SyntaxChecker check,
                                 ArrayList<SyntaxValue> expanded)
        throws FusionException
    {
        Binding b = verifyBoundId(moduleNamespace, identifier, check);

        BaseSymbol freeName = b.getName();
        if (freeName != identifier.getName())
        {
            String message =
                "cannot export binding since symbolic name " +
                printQuotedSymbol(identifier.stringValue()) +
                " differs from resolved name " +
                printQuotedSymbol(freeName.stringValue());
            throw check.failure(message);
        }

        expanded.add(identifier);
    }

    /**
     * Check that an identifier is bound within the module and can be exported.
     */
    private Binding verifyBoundId(Namespace moduleNamespace,
                                  SyntaxSymbol identifier,
                                  SyntaxChecker check)
        throws FusionException
    {
        // TODO FUSION-139 id.resolve works when the id has the ns in context
        // It doesn't work when the ns isn't in context because the
        // provided binding was macro-introduced.

        Binding b = identifier.resolve();
        // Binding local = moduleNamespace.localResolve(identifier);
        // localResolve isn't right either since it doesn't find imports

        if (b instanceof FreeBinding)
        {
            String message =
                "cannot export " + printQuotedSymbol(identifier.stringValue()) +
                " since it is neither defined in nor imported into this module.";
            throw check.failure(message, identifier);
        }

        return b;
    }

    private void expandProvideSexp(Evaluator eval,
                                   Namespace moduleNamespace,
                                   SyntaxSexp specForm,
                                   SyntaxChecker provideCheck,
                                   ArrayList<SyntaxValue> expanded)
        throws FusionException
    {
        SyntaxChecker check = new SyntaxChecker(eval, specForm);

        GlobalState globalState = eval.getGlobalState();

        Binding b = specForm.firstTargetBinding(eval);
        if (b == globalState.myKernelAllDefinedOutBinding)
        {
            // Expanded form is a sequence of identifiers.

            check.arityExact(1);

            SyntaxSymbol tag = (SyntaxSymbol) specForm.get(eval, 0);

            // Filter by lexical context: we shouldn't export identifiers
            // introduced by macros unless this form was also introduced
            // at the same time.
            for (NsDefinedBinding binding : moduleNamespace.getDefinedBindings())
            {
                // TODO FUSION-329 the datum->syntax context should be the sexp
                // form `(all_defined_out)` not just `all_defined_out` but
                // we don't currently retain context on SyntaxSexp after
                // it has been pushed down to children.
                SyntaxSymbol localized = (SyntaxSymbol)
                    datumToSyntax(eval,
                                  binding.getName(),
                                  tag,
                                  null);
                Binding localBinding = moduleNamespace.resolveDefinition(localized);
                if (localBinding != null && binding.sameTarget(localBinding))
                {
                    localized = localized.copyReplacingBinding(binding);
                    expanded.add(localized);
                }

                // TODO FUSION-136 handle rename-transformers per Racket
            }
        }
        else if (b == globalState.myKernelRenameOutBinding)
        {
            // Expanded form is a sequence of (rename local exported) triples.

            SyntaxSymbol renameSym = SyntaxSymbol.make(eval, null /*location*/,
                                                       makeSymbol(eval, "rename"));

            int arity = check.arityAtLeast(1);
            for (int i = 1; i < arity; i++)
            {
                SyntaxChecker rename = check.subformSexp("rename pair", i);
                rename.arityExact(2);

                SyntaxSymbol inner =
                    rename.requiredIdentifier("local identifier", 0);
                SyntaxSymbol outer =
                    rename.requiredIdentifier("exported identifier", 1);

                verifyBoundId(moduleNamespace, inner, check);

                expanded.add(SyntaxSexp.make(eval, renameSym, inner, outer));
            }
        }
        else
        {
            throw provideCheck.failure("invalid provide-spec");
        }
    }


    //========================================================================


    @Override
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        throw new IllegalStateException("Provide shouldn't be compiled");
    }


    //========================================================================


    /**
     * This class primarily provides syntax to bind to provide-clauses so that
     * {@link ProvideForm} can compare bindings, not symbolic names.  This is
     * as specified by Racket.
     */
    private abstract static class AbstractProvideClauseForm
        extends SyntacticForm
    {
        AbstractProvideClauseForm(String bodyPattern, String doc)
        {
            super(bodyPattern, doc);
        }

        @Override
        SyntaxValue expand(Expander expander, Environment env, SyntaxSexp stx)
            throws FusionException
        {
            throw check(expander, stx).failure("must be used inside `provide`");
        }

        @Override
        CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp stx)
            throws FusionException
        {
            throw new IllegalStateException("Shouldn't be compiled");
        }
    }


    static final class AllDefinedOutForm
        extends AbstractProvideClauseForm
    {
        AllDefinedOutForm()
        {
            //    "                                                                               |
            super("",
                  "A `provide` clause that exports all bindings `define`d by the enclosing module.\n" +
                  "Imported bindings are not exported.\n" +
                  "\n" +
                  "This form can only appear within `provide`.");
        }
    }


    static final class RenameOutForm
        extends AbstractProvideClauseForm
    {
        RenameOutForm()
        {
            //    "                                                                               |
            super("(local_id exported_id) ...",
                  "A `provide` clause that exports each `local_id` using the name `exported_id`.\n" +
                  "This effectively renames the binding on export.\n" +
                  "\n" +
                  "This form can only appear within `provide`.");
        }
    }
}
