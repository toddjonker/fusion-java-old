// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonList;
import com.amazon.ion.IonSexp;
import com.amazon.ion.ValueFactory;
import java.util.TreeSet;

/**
 * The {@code list_bindings} syntactic form.
 * This cannot be a procedure since it requires access to the caller's
 * lexical environment.
 */
final class ListBindingsKeyword
    extends KeywordValue
{
    ListBindingsKeyword()
    {
        //    "                                                                               |
        super(null,
              "Returns a list of symbols, one for each visible binding.");
    }


    @Override
    FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
    {
        TreeSet<String> names = new TreeSet<String>();
        env.collectNames(names);

        ValueFactory factory = expr.getSystem();
        IonList result = factory.newEmptyList();

        for (String name : names)
        {
            result.add(factory.newSymbol(name));
        }

        return new DomValue(result);
    }
}
