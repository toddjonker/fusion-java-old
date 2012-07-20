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
    FusionValue invoke(Evaluator eval, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        int size = stx.size();
        if (size != 2)
        {
            throw new SyntaxFailure(getEffectiveName(),
                                    "a single template required",
                                    stx);
        }

        SyntaxValue node = stx.get(1);
        return quasiSyntax(eval, env, node);
    }

    SyntaxValue quasiSyntax(Evaluator eval, Environment env, SyntaxValue stx)
        throws FusionException
    {
        if (stx instanceof SyntaxSexp)
        {
            return quasiSyntax(eval, env, (SyntaxSexp) stx);
        }
        else
        {
            return stx;
        }
    }

    SyntaxValue quasiSyntax(Evaluator eval, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        int size = stx.size();
        if (size == 0) return stx;

        if (size == 2)
        {
            SyntaxValue first = stx.get(0);
            if (first instanceof SyntaxSymbol
                && "unsyntax".equals(((SyntaxSymbol)first).stringValue()))
            {
                FusionValue unquoted = eval.eval(env, stx.get(1));

                // TODO type check
                return (SyntaxValue) unquoted;
            }
        }

        SyntaxValue[] children = new SyntaxValue[size];
        for (int i = 0; i < size; i++)
        {
            SyntaxValue orig = stx.get(i);
            SyntaxValue done = quasiSyntax(eval, env, orig);
            children[i] = done;
        }
        return SyntaxSexp.make(children);
    }
}
