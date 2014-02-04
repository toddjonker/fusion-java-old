// Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionBlob.isBlob;
import static com.amazon.fusion.FusionLob.isLob;
import static org.junit.Assert.assertTrue;

/**
 *
 */
public class BlobTest
    extends LobTestCase
{
    @Override
    Object makeLob(byte[] value)
        throws FusionException
    {
        TopLevel top = topLevel();
        return FusionBlob.forBytesNoCopy(top, value);
    }


    @Override
    void checkLobType(Object lob)
        throws FusionException
    {
        TopLevel top = topLevel();
        assertTrue(isLob(top, lob));
        assertTrue(isBlob(top, lob));
    }
}
