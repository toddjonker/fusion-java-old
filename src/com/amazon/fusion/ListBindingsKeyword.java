// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonList;
import com.amazon.ion.IonSexp;
import com.amazon.ion.ValueFactory;
import java.util.TreeSet;

final class ListBindingsKeyword
    extends KeywordValue
{
    ListBindingsKeyword(String keyword)
    {
        super(keyword);
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
            result.add(factory.newString(name));
        }

        return new DomValue(result);
    }
}
