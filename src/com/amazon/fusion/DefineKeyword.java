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
