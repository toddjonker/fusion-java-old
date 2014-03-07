// Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import com.amazon.ion.IonDatagram;
import com.amazon.ion.IonReader;
import com.amazon.ion.IonSystem;
import com.amazon.ion.system.IonSystemBuilder;
import org.junit.Test;


public class SourceLocationTest
{
    @Test
    public void testLocationDisplay()
    {
        IonSystem sys = IonSystemBuilder.standard().build();
        IonDatagram dg = sys.getLoader().load("(hi)");
        IonReader ir = sys.newReader(dg.getBytes());

        // Binary position, no name.
        ir.next();
        SourceLocation loc = SourceLocation.forCurrentSpan(ir, null);
        loc = SourceLocation.forCurrentSpan(ir, null);
        assertNull(loc);

        // Binary position, with name
        SourceName name = SourceName.forDisplay("test source");
        loc = SourceLocation.forCurrentSpan(ir, name);
        assertEquals("unknown location in test source",
                     loc.toString());

        // Text position, no name
        ir = sys.newReader("(hi)");
        ir.next();
        loc = SourceLocation.forCurrentSpan(ir, null);
        assertEquals("1st line, 1st column",
                     loc.toString());

        // Text position, with name
        loc = SourceLocation.forCurrentSpan(ir, name);
        assertEquals("1st line, 1st column of test source",
                     loc.toString());
        ir.stepIn();
        ir.next();
        loc = SourceLocation.forCurrentSpan(ir, name);
        assertEquals("1st line, 2nd column of test source",
                     loc.toString());
    }
}
