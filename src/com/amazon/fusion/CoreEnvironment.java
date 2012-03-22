// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.io.PrintWriter;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonSymbol;
import com.amazon.ion.IonValue;
import com.amazon.ion.ValueFactory;
import java.util.HashMap;
import java.util.Map;


public class CoreEnvironment
    implements Environment
{
    private ValueFactory myValueFactory;
    private Map<String,FusionValue> myBindings = 
        new HashMap<String,FusionValue>();
    
    
    private final FusionValue myDefineKeyword = new FusionValue() 
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
        
        @Override
        void print(PrintWriter out)
        {
            out.println("// Keyword 'define'");
        }
    };
    
    private final FusionValue myFuncKeyword = new FusionValue() 
    {
        @Override
        FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
        {
            return new FunctionValue(expr, env);
        }
        
        @Override
        void print(PrintWriter out)
        {
            out.println("// Keyword 'func'");
        }
    };
    
    CoreEnvironment(ValueFactory valueFactory)
    {
        myValueFactory = valueFactory;
        
        myBindings.put("fusion_version", 
                       FusionValue.forIon(myValueFactory.newString("0.1a1")));
        myBindings.put("define", myDefineKeyword);
        myBindings.put("func", myFuncKeyword);
    }

    public FusionValue lookup(String name)
    {
        return myBindings.get(name);
    }
}
