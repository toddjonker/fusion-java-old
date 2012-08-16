// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

final class DefineKeyword
    extends KeywordValue
{
    DefineKeyword()
    {
        //    "                                                                               |
        super("VAR VALUE",
              "Defines a top-level variable VAR with the given VALUE.");
    }


    static String boundName(Evaluator eval, Environment env, SyntaxSexp stx)
        throws SyntaxFailure
    {
        // TODO error checking
        SyntaxSymbol name = (SyntaxSymbol) stx.get(1);
        return name.stringValue();
    }


    @Override
    SyntaxValue prepare(Evaluator eval, Environment env, SyntaxSexp stx)
        throws SyntaxFailure
    {
        SyntaxChecker check = new SyntaxChecker(getInferredName(), stx);
        check.arityExact(3);
        SyntaxSymbol name = (SyntaxSymbol) stx.get(1);
        SyntaxValue valueStx = stx.get(2);

        Namespace ns = env.namespace();
        if (ns.lookup(name.stringValue()) == null)
        {
            // If at module top-level, this has already been done.
            ns.bind(name.stringValue(), UNDEF);
        }

        SyntaxValue expanded = valueStx.prepare(eval, env);
        if (expanded != valueStx) stx.set(2, expanded);

        return stx;
    }


    @Override
    FusionValue invoke(Evaluator eval, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        SyntaxChecker check = new SyntaxChecker(getInferredName(), stx);
        check.arityExact(3);
        SyntaxSymbol name = (SyntaxSymbol) stx.get(1);
        SyntaxValue valueStx = stx.get(2);

        FusionValue fusionValue = eval.eval(env, valueStx);

        Namespace ns = env.namespace();
        ns.bind(name.stringValue(), fusionValue);

        return fusionValue;
    }
}
