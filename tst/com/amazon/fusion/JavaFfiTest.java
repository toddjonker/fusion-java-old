// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.ion.util.IonTextUtils.printString;
import org.junit.Test;

/**
 *
 */
public class JavaFfiTest
    extends CoreTestCase
{
    public class Uninstantiable extends Procedure
    {
        public Uninstantiable()
            throws Exception
        {
            super("doc");
            throw new Exception("boom");
        }

        @Override
        FusionValue invoke(Evaluator eval, FusionValue[] args)
            throws FusionException
        {
            return null;
        }
    }


    private String name(Class<?> c)
    {
        String className = c.getName();
        return printString(className);
    }


    @Test
    public void testJavaNew()
        throws Exception
    {
        eval("(define plus (java_new " + name(SumProc.class) + "))");
        assertEval(2, "(plus 1 1)");
    }

    @Test
    public void testJavaNewBadArgs()
        throws Exception
    {
        expectContractFailure("(define foo (java_new '''no such class'''))");
        expectContractFailure("(define foo (java_new " + name(Uninstantiable.class) + "))");
   }
}
