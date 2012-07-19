// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonSymbol;
import com.amazon.ion.IonValue;

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
    FusionValue invoke(Evaluator eval, Environment env, IonSexp stx)
        throws FusionException
    {
        SyntaxChecker check = new SyntaxChecker(getInferredName(), stx);
        check.arityExact(3);
        IonSymbol name = check.requiredSymbol("identifier to define", 1);
        IonValue ionValue = stx.get(2);

        FusionValue fusionValue = eval.eval(env, ionValue);

        Namespace ns = env.namespace();
        ns.bind(name.stringValue(), fusionValue);

        return fusionValue;
    }
}
