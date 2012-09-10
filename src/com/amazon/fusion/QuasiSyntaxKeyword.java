// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.Namespace.NsBinding;
import com.amazon.fusion.QuoteSyntaxKeyword.CompiledQuoteSyntax;

final class QuasiSyntaxKeyword
    extends KeywordValue
{
    private final Binding myQsBinding;
    private final Binding myUsBinding;

    public QuasiSyntaxKeyword(FusionValue qsIdentifier,
                              FusionValue usIdentifier)
    {
        super("template", "...");

        SyntaxSymbol id = (SyntaxSymbol) qsIdentifier;
        myQsBinding = id.resolve();
        assert myQsBinding instanceof NsBinding;

        id = (SyntaxSymbol) usIdentifier;
        myUsBinding = id.resolve();
        assert myUsBinding instanceof NsBinding;
    }


    @Override
    SyntaxValue prepare(Evaluator eval, Environment env, SyntaxSexp stx)
        throws SyntaxFailure
    {
        if (stx.size() != 2)
        {
            throw new SyntaxFailure(getEffectiveName(),
                                    "a single template required",
                                    stx);
        }

        SyntaxValue subform = stx.get(1);
        subform = quasiPrepare(eval, env, subform, 0);

        stx = SyntaxSexp.make(stx.getLocation(), stx.get(0), subform);
        return stx;
    }

    SyntaxValue quasiPrepare(Evaluator eval, Environment env, SyntaxValue stx,
                             int depth)
        throws SyntaxFailure
    {
        if (stx instanceof SyntaxSexp)
        {
            return quasiPrepare(eval, env, (SyntaxSexp) stx, depth);
        }
        else
        {
            return stx;
        }
    }

    SyntaxValue quasiPrepare(Evaluator eval, Environment env, SyntaxSexp stx,
                             int depth)
        throws SyntaxFailure
    {
        int size = stx.size();
        if (size == 0) return stx;

        SyntaxValue[] children = stx.extract();
        SyntaxValue first = children[0];
        if (first instanceof SyntaxSymbol)
        {
            // Be careful that we don't force a binding too early.
            Binding binding = ((SyntaxSymbol)first).uncachedResolve();
            binding = binding.originalBinding();

            if (myUsBinding == binding)
            {
                check(stx).arityExact(2);

                if (depth < 1)
                {
                    SyntaxValue subform = children[1];
                    subform = subform.prepare(eval, env);
                    stx = SyntaxSexp.make(stx.getLocation(), children);
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

        for (int i = 0; i < size; i++)
        {
            SyntaxValue subform = stx.get(i);
            children[i] = quasiPrepare(eval, env, subform, depth);
        }

        stx = SyntaxSexp.make(stx.getLocation(), children);
        return stx;
    }


    //========================================================================


    @Override
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp source)
        throws FusionException
    {
        SyntaxValue node = source.get(1);
        return compileQuasi(eval, env, node, 0);
    }


    private CompiledForm compileQuasi(Evaluator eval, Environment env,
                                      SyntaxValue source, int depth)
        throws FusionException
    {
        if (source instanceof SyntaxSexp)
        {
            return compileQuasi(eval, env, (SyntaxSexp) source, depth);
        }
        else
        {
            return new CompiledQuoteSyntax(source);
        }
    }


    private CompiledForm compileQuasi(Evaluator eval, Environment env,
                                      SyntaxSexp source, int depth)
        throws FusionException
    {
        int size = source.size();
        if (size == 0) return new CompiledQuoteSyntax(source);

        if (size == 2)
        {
            SyntaxValue first = source.get(0);
            if (first instanceof SyntaxSymbol)
            {
                Binding binding = ((SyntaxSymbol)first).uncachedResolve();
                binding = binding.originalBinding();

                if (myUsBinding == binding)
                {
                    if (depth == 0)
                    {
                        SyntaxValue unquotedSyntax = source.get(1);
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

        boolean same = true;
        CompiledForm[] children = new CompiledForm[size];
        for (int i = 0; i < size; i++)
        {
            SyntaxValue orig = source.get(i);
            children[i] = compileQuasi(eval, env, orig, depth);
            same &= (children[i] instanceof CompiledQuoteSyntax);
        }

        if (same)
        {
            // There's no unsyntax within the children, so use the original.
            return new CompiledQuoteSyntax(source);
        }

        return new CompiledQuasiSyntaxSexp(source.getLocation(), children);
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
        public SyntaxValue doExec(Evaluator eval, Store store)
            throws FusionException
        {
            int size = myChildForms.length;
            SyntaxValue[] children = new SyntaxValue[size];
            for (int i = 0; i < size; i++)
            {
                Object child = eval.exec(store, myChildForms[i]);

                // This cast is safe because children are either quote-syntax
                // or unquote, which always return syntax.
                children[i] = (SyntaxValue) child;
            }
            return SyntaxSexp.make(myLocation, children);
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
        public SyntaxValue doExec(Evaluator eval, Store store)
            throws FusionException
        {
            Object unquoted = eval.exec(store, myUnquotedForm);
            try
            {
                return (SyntaxValue) unquoted;
            }
            catch (ClassCastException e) {}

            String message =
                "Result of " + writeToString(myUnquotedSyntax) +
                " isn't a syntax value: " +
                writeToString(unquoted);
            throw new ContractFailure(message);
        }
    }
}
