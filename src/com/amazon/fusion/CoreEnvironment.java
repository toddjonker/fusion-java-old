// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonSymbol;
import com.amazon.ion.IonValue;
import com.amazon.ion.ValueFactory;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * The core, built-in bindings for Fusion.
 * This is kind-of hacky and will probably be refactored significantly.
 */
class CoreEnvironment
    implements Environment
{
    private final class DefineKeyword
        extends KeywordValue
    {
        private DefineKeyword(String keyword)
        {
            super(keyword, "VAR VALUE",
                  "Defines a global variable VAR with the given VALUE.");
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
        myBindings.put("doc",
                       new DocFunction());
        myBindings.put(".", new DotFunction());
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
