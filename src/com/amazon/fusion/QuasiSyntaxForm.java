// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionWrite.safeWriteToString;
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
        if (stx.size() != 2)
        {
            throw new SyntaxFailure(identify(),
                                    "a single template required",
                                    stx);
        }

        SyntaxValue subform = stx.get(1);
        subform = expand(expander, env, subform, 0);

        stx = SyntaxSexp.make(expander, stx.getLocation(),
                              stx.get(0), subform);
        return stx;
    }

    private SyntaxValue expand(Expander expander, Environment env,
                               SyntaxValue stx, int depth)
        throws FusionException
    {
        // TODO FUSION-46 handle unsyntax inside lists and structs
        if (stx instanceof SyntaxSexp)
        {
            return expand(expander, env, (SyntaxSexp) stx, depth);
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
        int size = stx.size();
        if (size == 0) return stx;

        SyntaxValue[] children = stx.extract();
        SyntaxValue first = children[0];
        if (first instanceof SyntaxSymbol)
        {
            // Be careful that we don't force a binding too early.
            Binding binding = ((SyntaxSymbol)first).uncachedResolveMaybe();
            if (binding != null)
            {
                binding = binding.originalBinding();

                if (myUsBinding == binding)
                {
                    check(stx).arityExact(2);

                    if (depth < 1)
                    {
                        SyntaxValue subform = children[1];
                        children[1] = expander.expandExpression(env, subform);
                        stx = SyntaxSexp.make(expander, stx.getLocation(),
                                              children);
                        return stx;
                    }

                    depth--;
                }
                else if (myQsBinding == binding)
                {
                    check(stx).arityExact(2);

                    depth++;
                }
            }
        }

        for (int i = 0; i < size; i++)
        {
            SyntaxValue subform = stx.get(i);
            children[i] = expand(expander, env, subform, depth);
        }

        stx = SyntaxSexp.make(expander, stx.getLocation(), children);
        return stx;
    }


    //========================================================================


    @Override
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        SyntaxValue node = stx.get(1);
        return compile(eval, env, node, 0);
    }


    private CompiledForm compile(Evaluator eval, Environment env,
                                 SyntaxValue stx, int depth)
        throws FusionException
    {
        if (stx instanceof SyntaxSexp)
        {
            return compile(eval, env, (SyntaxSexp) stx, depth);
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

        if (size == 2)
        {
            SyntaxValue first = stx.get(0);
            if (first instanceof SyntaxSymbol)
            {
                Binding binding = ((SyntaxSymbol)first).uncachedResolveMaybe();
                if (binding != null)
                {
                    binding = binding.originalBinding();

                    if (myUsBinding == binding)
                    {
                        if (depth == 0)
                        {
                            SyntaxValue unquotedSyntax = stx.get(1);
                            CompiledForm unquotedForm =
                                eval.compile(env, unquotedSyntax);
                            return new CompiledUnsyntax(unquotedSyntax,
                                                        unquotedForm);
                        }
                        depth--;
                    }
                    else if (myQsBinding == binding)
                    {
                        depth++;
                    }
                }
            }
        }

        boolean same = true;
        CompiledForm[] children = new CompiledForm[size];
        for (int i = 0; i < size; i++)
        {
            SyntaxValue orig = stx.get(i);
            children[i] = compile(eval, env, orig, depth);
            same &= (children[i] instanceof CompiledQuoteSyntax);
        }

        if (same)
        {
            // There's no unsyntax within the children, so use the original.
            return new CompiledQuoteSyntax(stx);
        }

        return new CompiledQuasiSyntaxSexp(stx.getLocation(), children);
    }


    //========================================================================


    private static final class CompiledQuasiSyntaxSexp
        implements CompiledForm
    {
        private final SourceLocation myLocation;
        private final CompiledForm[] myChildForms;

        CompiledQuasiSyntaxSexp(SourceLocation location,
                                CompiledForm[] childForms)
        {
            myLocation   = location;
            myChildForms = childForms;
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
                // or unquote, which always return syntax.
                children[i] = (SyntaxValue) child;
            }
            return SyntaxSexp.make(eval, myLocation, children);
        }
    }


    private static final class CompiledUnsyntax
        implements CompiledForm
    {
        // TODO FUSION-35 we shouldn't retain the whole source
        private final SyntaxValue  myUnquotedSyntax;
        private final CompiledForm myUnquotedForm;

        CompiledUnsyntax(SyntaxValue  unquotedSyntax,
                         CompiledForm unquotedForm)
        {
            myUnquotedSyntax = unquotedSyntax;
            myUnquotedForm   = unquotedForm;
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
                "Result of (unsyntax " +
                safeWriteToString(eval, myUnquotedSyntax) +
                ") isn't a syntax value: " +
                safeWriteToString(eval, unquoted);
            throw new ContractFailure(message);
        }
    }
}
