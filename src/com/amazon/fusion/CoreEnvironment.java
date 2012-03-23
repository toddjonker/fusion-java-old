// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonSymbol;
import com.amazon.ion.IonValue;
import com.amazon.ion.ValueFactory;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

class CoreEnvironment
    implements Environment
{
    private final class DefineKeyword
        extends KeywordValue
    {
        private DefineKeyword(String keyword)
        {
            super(keyword);
        }

        @Override
        FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
        {
            IonSymbol name = (IonSymbol) expr.get(1);
            IonValue ionValue = expr.get(2);

            FusionValue fusionValue = eval.eval(env, ionValue);
            myBindings.put(name.stringValue(), fusionValue);

            return fusionValue;
        }
    }


    private final ValueFactory myValueFactory;
    private final Map<String,FusionValue> myBindings =
        new HashMap<String,FusionValue>();


    CoreEnvironment(ValueFactory valueFactory)
    {
        myValueFactory = valueFactory;

        myBindings.put("fusion_version",
                       new DomValue(myValueFactory.newString("0.1a1")));
        myBindings.put("define",
                       new DefineKeyword("define"));
        myBindings.put("func",
                       new FuncKeyword("func"));
        myBindings.put("list_bindings",
                       new ListBindingsKeyword("list_bindings"));
    }

    @Override
    public FusionValue lookup(String name)
    {
        return myBindings.get(name);
    }

    @Override
    public void collectNames(Collection<String> names)
    {
        names.addAll(myBindings.keySet());
    }
}
