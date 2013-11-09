// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionIo.safeWriteToString;
import com.amazon.fusion.QuoteSyntaxForm.CompiledQuoteSyntax;

final class QuasiSyntaxForm
    extends SyntacticForm
{
    private final Binding myQsBinding;
    private final Binding myUsBinding;

    public QuasiSyntaxForm(Object qsIdentifier,
                           Object usIdentifier)
    {
        super("template", "...");

        SyntaxSymbol id = (SyntaxSymbol) qsIdentifier;
        myQsBinding = id.resolve();

        id = (SyntaxSymbol) usIdentifier;
        myUsBinding = id.resolve();
    }


    @Override
    SyntaxValue expand(Expander expander, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        final Evaluator eval = expander.getEvaluator();

        if (stx.size() != 2)
        {
            throw new SyntaxException(identify(),
                                      "a single template required",
                                      stx);
        }

        SyntaxValue subform = stx.get(eval, 1);
        subform = expand(expander, env, subform, 0);

        stx = SyntaxSexp.make(expander, stx.getLocation(),
                              stx.get(eval, 0), subform);
        return stx;
    }

    private SyntaxValue expand(Expander expander, Environment env,
                               SyntaxValue stx, int depth)
        throws FusionException
    {
        // TODO FUSION-225 handle unsyntax inside structs
        if (stx instanceof SyntaxSexp)
        {
            return expand(expander, env, (SyntaxSexp) stx, depth);
        }
        else if (stx instanceof SyntaxList)
        {
            return expand(expander, env, (SyntaxList) stx, depth);
        }
        else
        {
            return stx;
        }
    }

    private SyntaxValue expand(Expander expander, Environment env,
                               SyntaxSexp stx, int depth)
        throws FusionException
    {
        final Evaluator eval = expander.getEvaluator();

        int size = stx.size();
        if (size == 0) return stx;

        SyntaxValue[] children = stx.extract(eval);

        Binding binding = stx.firstBindingMaybe(eval);
        if (myUsBinding == binding)
        {
            check(eval, stx).arityExact(2);

            if (depth < 1)
            {
                SyntaxValue subform = children[1];
                children[1] = expander.expandExpression(env, subform);

                // TODO accept annotations on unsyntax form?
                if (stx.getAnnotations().length != 0)
                {
                    String message =
                        "Annotations not accepted on unsyntax form";
                    throw check(eval, stx).failure(message);
                }

                return SyntaxSexp.make(eval, stx.getLocation(), children);
            }

            depth--;
        }
        else if (myQsBinding == binding)
        {
            check(eval, stx).arityExact(2);
            depth++;
        }

        boolean same = true;
        for (int i = 0; i < size; i++)
        {
            SyntaxValue subform = stx.get(eval, i);
            SyntaxValue expanded = expand(expander, env, subform, depth);
            same &= (subform == expanded);
            children[i] = expanded;
        }

        if (same) return stx;

        return SyntaxSexp.make(eval, stx.getLocation(), stx.getAnnotations(),
                               children);
    }


    private SyntaxValue expand(Expander expander, Environment env,
                               SyntaxList stx, int depth)
        throws FusionException
    {
        final Evaluator eval = expander.getEvaluator();

        int size = stx.size();
        if (size == 0) return stx;

        boolean same = true;

        SyntaxValue[] children = stx.extract(eval);
        for (int i = 0; i < size; i++)
        {
            SyntaxValue subform = children[i];
            SyntaxValue expanded = expand(expander, env, subform, depth);
            same &= (subform == expanded);
            children[i] = expanded;
        }

        if (same) return stx;

        return SyntaxList.make(stx.getLocation(),
                               stx.getAnnotations(),
                               children);
    }


    //========================================================================


    @Override
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        SyntaxValue node = stx.get(eval, 1);
        return compile(eval, env, node, 0);
    }


    private CompiledForm compile(Evaluator eval, Environment env,
                                 SyntaxValue stx, int depth)
        throws FusionException
    {
        // TODO FUSION-225 handle unsyntax inside structs
        if (stx instanceof SyntaxSexp)
        {
            return compile(eval, env, (SyntaxSexp) stx, depth);
        }
        else if (stx instanceof SyntaxList)
        {
            return compile(eval, env, (SyntaxList) stx, depth);
        }
        else
        {
            return new CompiledQuoteSyntax(stx);
        }
    }


    private CompiledForm compile(Evaluator eval, Environment env,
                                 SyntaxSexp stx, int depth)
        throws FusionException
    {
        int size = stx.size();
        if (size == 0) return new CompiledQuoteSyntax(stx);

        // Look for an (unsyntax ...) form
        if (size == 2)
        {
            Binding binding = stx.firstBindingMaybe(eval);
            if (myUsBinding == binding)
            {
                if (depth == 0)
                {
                    assert stx.getAnnotations().length == 0;
                    SyntaxValue unquotedSyntax = stx.get(eval, 1);
                    CompiledForm unquotedForm =
                        eval.compile(env, unquotedSyntax);

                    SourceLocation location = unquotedSyntax.getLocation();
                    String expression =
                        safeWriteToString(eval, unquotedSyntax);

                    return new CompiledUnsyntax(unquotedForm,
                                                location,
                                                expression);
                }
                depth--;
            }
            else if (myQsBinding == binding)
            {
                depth++;
            }
        }

        boolean same = true;
        CompiledForm[] children = new CompiledForm[size];
        for (int i = 0; i < size; i++)
        {
            SyntaxValue orig = stx.get(eval, i);
            children[i] = compile(eval, env, orig, depth);
            same &= (children[i] instanceof CompiledQuoteSyntax);
        }

        if (same)
        {
            // There's no unsyntax within the children, so use the original.
            return new CompiledQuoteSyntax(stx);
        }

        return new CompiledQuasiSyntaxSexp(stx.getLocation(),
                                           stx.getAnnotations(),
                                           children);
    }


    private CompiledForm compile(Evaluator eval, Environment env,
                                 SyntaxList stx, int depth)
        throws FusionException
    {
        int size = stx.size();
        if (size == 0) return new CompiledQuoteSyntax(stx);

        boolean same = true;
        CompiledForm[] children = new CompiledForm[size];
        for (int i = 0; i < size; i++)
        {
            SyntaxValue orig = stx.get(eval, i);
            children[i] = compile(eval, env, orig, depth);
            same &= (children[i] instanceof CompiledQuoteSyntax);
        }

        if (same)
        {
            // There's no unsyntax within the children, so use the original.
            return new CompiledQuoteSyntax(stx);
        }

        return new CompiledQuasiSyntaxList(stx.getLocation(),
                                           stx.getAnnotations(),
                                           children);
    }


    //========================================================================


    private static final class CompiledQuasiSyntaxSexp
        implements CompiledForm
    {
        private final SourceLocation myLocation;
        private final String[]       myAnnotations;
        private final CompiledForm[] myChildForms;

        CompiledQuasiSyntaxSexp(SourceLocation location,
                                String[]       annotations,
                                CompiledForm[] childForms)
        {
            myLocation    = location;
            myAnnotations = annotations;
            myChildForms  = childForms;
        }

        @Override
        public SyntaxValue doEval(Evaluator eval, Store store)
            throws FusionException
        {
            int size = myChildForms.length;
            SyntaxValue[] children = new SyntaxValue[size];
            for (int i = 0; i < size; i++)
            {
                Object child = eval.eval(store, myChildForms[i]);

                // This cast is safe because children are either quote-syntax
                // or unsyntax, which always return syntax.
                children[i] = (SyntaxValue) child;
            }
            return SyntaxSexp.make(eval, myLocation, myAnnotations, children);
        }
    }


    private static final class CompiledQuasiSyntaxList
        implements CompiledForm
    {
        private final SourceLocation myLocation;
        private final String[]       myAnnotations;
        private final CompiledForm[] myChildForms;

        CompiledQuasiSyntaxList(SourceLocation location,
                                String[]       annotations,
                                CompiledForm[] childForms)
        {
            myLocation    = location;
            myAnnotations = annotations;
            myChildForms  = childForms;
        }

        @Override
        public SyntaxValue doEval(Evaluator eval, Store store)
            throws FusionException
        {
            int size = myChildForms.length;
            SyntaxValue[] children = new SyntaxValue[size];
            for (int i = 0; i < size; i++)
            {
                Object child = eval.eval(store, myChildForms[i]);

                // This cast is safe because children are either quote-syntax
                // or unsyntax, which always return syntax.
                children[i] = (SyntaxValue) child;
            }
            return SyntaxList.make(myLocation, myAnnotations, children);
        }
    }


    private static final class CompiledUnsyntax
        implements CompiledForm
    {
        private final CompiledForm myUnquotedForm;
        private final SourceLocation myLocation;
        private final String         myExpression;

        CompiledUnsyntax(CompiledForm unquotedForm,
                         SourceLocation location,
                         String expression)
        {
            myUnquotedForm   = unquotedForm;
            myLocation       = location;
            myExpression     = expression;
        }

        @Override
        public SyntaxValue doEval(Evaluator eval, Store store)
            throws FusionException
        {
            Object unquoted = eval.eval(store, myUnquotedForm);
            try
            {
                return (SyntaxValue) unquoted;
            }
            catch (ClassCastException e) {}

            String message =
                "Result of (unsyntax " + myExpression +
                ") isn't a syntax value: " +
                safeWriteToString(eval, unquoted);
            throw new ContractException(message, myLocation);
        }
    }
}
