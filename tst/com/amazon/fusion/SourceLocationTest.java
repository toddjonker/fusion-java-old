// Copyright (c) 2014-2019 Amazon.com, Inc.  All rights reserved.

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
    private void assertNoLocation(IonReader ir)
    {
        SourceLocation loc = SourceLocation.forCurrentSpan(ir, null);
        assertNull("expected null SourceLocation", loc);

        SourceName name = SourceName.forDisplay("test source");
        loc = SourceLocation.forCurrentSpan(ir, name);
        assertEquals("unknown location in test source", loc.display());
    }

    private void assertLocation(String expectedOffsets, IonReader ir)
    {
        SourceLocation loc = SourceLocation.forCurrentSpan(ir, null);
        assertEquals(expectedOffsets, loc.display());

        SourceName name = SourceName.forDisplay("test source");
        loc = SourceLocation.forCurrentSpan(ir, name);
        assertEquals(expectedOffsets + " of test source", loc.display());
    }


    @Test
    public void testLocationDisplay()
    {
        IonSystem sys = IonSystemBuilder.standard().build();
        IonDatagram dg = sys.getLoader().load("(hi)");

        // Binary reader doesn't display offsets.
        // TODO Display offsets in binary data.

        IonReader ir = sys.newReader(dg.getBytes());

        assertNoLocation(ir);  // Before first value

        ir.next();
        assertNoLocation(ir);

        ir.stepIn();
        assertNoLocation(ir);  // Before first child


        // Text reader gives line/column locations

        ir = sys.newReader("(hi)");

        assertNoLocation(ir);  // Before first value

        ir.next();
        assertLocation("1st line, 1st column", ir);

        ir.stepIn();
        assertNoLocation(ir);  // Before first child

        ir.next();
        assertLocation("1st line, 2nd column", ir);

        ir.next();
        assertNoLocation(ir);  // After last child
    }
}
