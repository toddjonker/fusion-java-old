// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonSymbol;
import com.amazon.ion.IonValue;
import java.util.Collection;


class FunctionValue
    extends FusionValue
{
    private final Environment myEnclosure;
    private final String myParam;
    
    FunctionValue(IonSexp definition, Environment enclosure)
    {
        super(definition);
        
        myEnclosure = enclosure;
        
        IonSymbol param = (IonSymbol) definition.get(1);
        myParam = param.stringValue();
    }
    
    @Override
    FusionValue invoke(Evaluator eval, final Environment env, IonSexp expr)
    {
        IonSexp funcDom = (IonSexp) getDom();
        IonValue body = funcDom.get(2);
        
        IonValue argumentExpr = expr.get(1);
        
        final FusionValue argumentValue = eval.eval(env, argumentExpr);
        
        Environment c2 = new Environment()
        {
            public FusionValue lookup(String name)
            {
                if (name.equals(myParam))
                {
                    return argumentValue;
                }

                return myEnclosure.lookup(name);
            }

            public void collectNames(Collection<String> names)
            {
                names.add(myParam);
                myEnclosure.collectNames(names);
            }
        };
        
        return eval.eval(c2, body);
    }
}
