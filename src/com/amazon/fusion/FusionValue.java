// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonException;
import com.amazon.ion.IonSexp;
import com.amazon.ion.IonValue;
import java.io.PrintWriter;


class FusionValue
{
    private final IonValue myDom;
    
    static FusionValue forIon(IonValue dom)
    {
        return new FusionValue(dom);
    }
    

    FusionValue()
    {
        myDom = null;
    }
    
    FusionValue(IonValue dom)
    {
        myDom = dom;
    }
    
    
    IonValue getDom()
    {
        return myDom;
    }
    
    
    void print(PrintWriter out)
    {
        out.println(myDom);
    }
    
    FusionValue invoke(Evaluator eval, Environment context, IonSexp expression)
    {
        throw new IonException("not invokable: " + myDom);
    }
}
