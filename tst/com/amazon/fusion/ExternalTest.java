// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonValue;
import java.io.File;
import java.io.FileInputStream;
import java.util.Iterator;
import org.junit.Ignore;
import org.junit.Test;

/**
 *
 */
public class ExternalTest
    extends CoreTestCase
{
    public static File getProjectHome()
    {
        String basedir = System.getProperty("user.dir");
        return new File(basedir);
    }

    public static File getProjectFile(String path)
    {
        return new File(getProjectHome(), path);
    }

    @Test @Ignore
    public void testSelfEval()
        throws Exception
    {
        File testSrc = getProjectFile("ftst/selfeval.ion");
        FileInputStream in = new FileInputStream(testSrc);
        Iterator<IonValue> clauses = system().iterate(in);
        do {
            IonValue expr = clauses.next();
            assertEval(expr, expr);
        }
        while (clauses.hasNext());
    }
}
