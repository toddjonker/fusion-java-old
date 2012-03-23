// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonSymbol;
import com.amazon.ion.IonValue;
import java.util.Collection;


class FuncValue
    extends FunctionValue
{
    private final Environment myEnclosure;
    private final String myParam;
    
    FuncValue(IonSexp definition, Environment enclosure)
    {
        super(definition);
        
        myEnclosure = enclosure;
        
        IonSymbol param = (IonSymbol) definition.get(1);
        myParam = param.stringValue();
    }
    
    @Override
    FusionValue invoke(Evaluator eval, final FusionValue argumentValue)
    {
        IonSexp funcDom = (IonSexp) getDom();
        IonValue body = funcDom.get(2);
                
        Environment c2 = new Environment()
        {
            @Override
            public FusionValue lookup(String name)
            {
                if (name.equals(myParam))
                {
                    return argumentValue;
                }

                return myEnclosure.lookup(name);
            }

            @Override
            public void collectNames(Collection<String> names)
            {
                names.add(myParam);
                myEnclosure.collectNames(names);
            }
        };
        
        return eval.eval(c2, body);
    }
}
