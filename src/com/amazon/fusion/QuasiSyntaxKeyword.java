// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.Namespace.NsBinding;

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


    @Override
    FusionValue invoke(Evaluator eval, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        SyntaxValue node = stx.get(1);
        return invokeQuasi(eval, env, node, 0);
    }

    SyntaxValue invokeQuasi(Evaluator eval, Environment env, SyntaxValue stx,
                            int depth)
        throws FusionException
    {
        if (stx instanceof SyntaxSexp)
        {
            return invokeQuasi(eval, env, (SyntaxSexp) stx, depth);
        }
        else
        {
            return stx;
        }
    }

    SyntaxValue invokeQuasi(Evaluator eval, Environment env, SyntaxSexp stx,
                            int depth)
        throws FusionException
    {
        int size = stx.size();
        if (size == 0) return stx;

        if (size == 2)
        {
            SyntaxValue first = stx.get(0);
            if (first instanceof SyntaxSymbol)
            {
                Binding binding = ((SyntaxSymbol)first).uncachedResolve();
                binding = binding.originalBinding();

                if (myUsBinding == binding)
                {
                    if (depth == 0)
                    {
                        FusionValue unquoted = eval.eval(env, stx.get(1));
                        try
                        {
                            return (SyntaxValue) unquoted;
                        }
                        catch (ClassCastException e) {}

                        String message =
                            "Result of " + writeToString(stx) +
                            " isn't a syntax value: " +
                            writeToString(unquoted);
                        throw new ContractFailure(message);
                    }
                    depth--;
                }
                else if (myQsBinding == binding)
                {
                    depth++;
                }
            }
        }

        SyntaxValue[] children = new SyntaxValue[size];
        for (int i = 0; i < size; i++)
        {
            SyntaxValue orig = stx.get(i);
            children[i] = invokeQuasi(eval, env, orig, depth);
        }
        return SyntaxSexp.make(stx.getLocation(), children);
    }
}
