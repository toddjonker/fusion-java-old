// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

final class QuasiSyntaxKeyword
    extends KeywordValue
{
    QuasiSyntaxKeyword()
    {
        super("template", "...");
        // TODO Auto-generated constructor stub
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
        stx.pushAnyWraps();                // Gets rid of wraps when size == 0
        int size = stx.size();
        if (size == 0) return stx;

        SyntaxValue[] children = stx.extract();
        SyntaxValue first = children[0];
        if (first instanceof SyntaxSymbol)
        {
            // TODO test using bindings
            String name = ((SyntaxSymbol)first).stringValue();
            if ("unsyntax".equals(name))
            {
                SyntaxChecker check = new SyntaxChecker("quasisyntax", stx);
                check.arityExact(2);

                if (depth < 1)
                {
                    SyntaxValue subform = children[1];
                    subform = subform.prepare(eval, env);
                    stx = SyntaxSexp.make(stx.getLocation(), children);
                    return stx;
                }

                depth--;
            }
            else if ("quasisyntax".equals(name))
            {
                SyntaxChecker check = new SyntaxChecker("quasisyntax", stx);
                check.arityExact(2);

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
                // TODO test using bindings
                String name = ((SyntaxSymbol)first).stringValue();
                if ("unsyntax".equals(name))
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
                            "Result of " + displayToString(stx) +
                            " isn't a syntax value: " +
                            displayToString(unquoted);
                        throw new ContractFailure(message);
                    }
                    depth--;
                }
                else if ("quasisyntax".equals(name))
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
