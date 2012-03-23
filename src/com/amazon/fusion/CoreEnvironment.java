// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonList;
import com.amazon.ion.IonSexp;
import com.amazon.ion.IonSymbol;
import com.amazon.ion.IonValue;
import com.amazon.ion.ValueFactory;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeSet;


class CoreEnvironment
    implements Environment
{
    private ValueFactory myValueFactory;
    private Map<String,FusionValue> myBindings = 
        new HashMap<String,FusionValue>();
    
    
    private final FusionValue myDefineKeyword = new KeywordValue("define") 
    {
        @Override
        FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
        {
            IonSymbol name = (IonSymbol) expr.get(1);
            IonValue ionValue = expr.get(2);
            
            FusionValue fusionValue = eval.eval(env, ionValue);
            myBindings.put(name.stringValue(), fusionValue);
            
            return fusionValue;
        }
    };

    private final FusionValue myListBindingsKeyword = 
        new KeywordValue("list_bindings")
    {
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
            
            return new FusionValue(result);
        }
    };

    private final FusionValue myFuncKeyword = new KeywordValue("func") 
    {
        @Override
        FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
        {
            return new FuncValue(expr, env);
        }
    };
    
    CoreEnvironment(ValueFactory valueFactory)
    {
        myValueFactory = valueFactory;
        
        myBindings.put("fusion_version", 
                       FusionValue.forIon(myValueFactory.newString("0.1a1")));
        myBindings.put("define", myDefineKeyword);
        myBindings.put("func", myFuncKeyword);
        myBindings.put("list_bindings", myListBindingsKeyword);
    }

    public FusionValue lookup(String name)
    {
        return myBindings.get(name);
    }
    
    public void collectNames(Collection<String> names)
    {
        names.addAll(myBindings.keySet());
    }
}
