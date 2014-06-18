// Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion._Private_CoverageCollectorImpl.SRCLOC_COMPARE;
import com.amazon.ion.IonReader;
import com.amazon.ion.IonSystem;
import com.amazon.ion.IonType;
import com.amazon.ion.OffsetSpan;
import com.amazon.ion.Span;
import com.amazon.ion.SpanProvider;
import com.amazon.ion.Timestamp;
import com.amazon.ion.system.IonSystemBuilder;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 *
 */
public final class _Private_CoverageWriter
{
    private final static String CSS =
        "body { color:black }" +
        ".covered { color:green }";


    private final IonSystem mySystem = IonSystemBuilder.standard().build();
    private final _Private_CoverageCollectorImpl myCollector;
    private final File mySourceFile;

    private final int BUFFER_SIZE = 2048;
    private final byte[] myCopyBuffer = new byte[BUFFER_SIZE];

    private long myIonBytesRead;
    private HtmlWriter myHtml;
    private boolean coverageState;

    private long myCoveredExpressions;
    private long myUncoveredExpressions;


    public _Private_CoverageWriter(_Private_CoverageCollectorImpl collector,
                                   File sourceFile)
    {
        myCollector  = collector;
        mySourceFile = sourceFile;
    }


    private void copySourceThroughOffset(InputStream source, long offset)
        throws IOException
    {
        long bytesToCopy = offset - myIonBytesRead;

        long bytesCopied = 0;
        while (bytesCopied < bytesToCopy)
        {
            int toRead = (int) Math.min(bytesToCopy - bytesCopied, BUFFER_SIZE);

            int bytesRead = source.read(myCopyBuffer, 0, toRead);

            if (bytesRead < 0) break; // EOF

            myHtml.write(myCopyBuffer, 0, bytesRead);
            bytesCopied += bytesRead;
        }

        myIonBytesRead += bytesCopied;
    }


    private void copySourceThroughCurrentOffset(InputStream source,
                                                SpanProvider spanProvider)
        throws IOException
    {
        Span span = spanProvider.currentSpan();
        OffsetSpan offsetSpan = span.asFacet(OffsetSpan.class);
        long offset = offsetSpan.getStartOffset();
        copySourceThroughOffset(source, offset);
    }


    private void setCoverageState(InputStream source,
                                  SpanProvider spanProvider,
                                  boolean covered)
        throws IOException
    {
        if (covered != coverageState)
        {
            copySourceThroughCurrentOffset(source, spanProvider);

            myHtml.append("</span><span class='");
            if (! covered)
            {
                myHtml.append("un");
            }
            myHtml.append("covered'>");
        }

        coverageState = covered;
    }


    private void renderSource(SourceName name)
        throws IOException
    {
        myHtml.append("<h1>");
        myHtml.append(name.display());
        myHtml.append("</h1>\n");

        SourceLocation[] locations = myCollector.sortedLocations(name);
        assert locations.length != 0;

        int locationIndex = 0;

        myHtml.append("<pre>");

        try (InputStream myIonBytes = new FileInputStream(name.getFile()))
        {
            myIonBytesRead = 0;

            try (IonReader ionReader =
                    mySystem.newReader(new FileInputStream(name.getFile())))
            {
                SpanProvider spanProvider =
                    ionReader.asFacet(SpanProvider.class);

                // We always start with a span so we can always end with one,
                // regardless of the data in between.
                coverageState = false;
                myHtml.append("<span class='uncovered'>");

                for (IonType t = ionReader.next(); t != null; )
                {
                    // Determine whether this value has been covered.
                    SourceLocation currentLoc =
                        SourceLocation.forCurrentSpan(ionReader, null);

                    SourceLocation coverageLoc = locations[locationIndex];

                    // We shouldn't skip past a known location.
                    assert SRCLOC_COMPARE.compare(currentLoc, coverageLoc) <= 0;

                    if (SRCLOC_COMPARE.compare(currentLoc, coverageLoc) == 0)
                    {
                        boolean covered =
                            myCollector.locationCovered(coverageLoc);
                        setCoverageState(myIonBytes, spanProvider, covered);
                        locationIndex++;

                        if (covered)
                        {
                            myCoveredExpressions++;
                        }
                        else
                        {
                            myUncoveredExpressions++;
                        }

                        if (locationIndex == locations.length) break;
                    }

                    if (IonType.isContainer(t))
                    {
                        ionReader.stepIn();
                    }

                    while ((t = ionReader.next()) == null
                           && ionReader.getDepth() != 0)
                    {
                        ionReader.stepOut();
                    }
                }

                // Copy the rest of the Ion source.
                copySourceThroughOffset(myIonBytes, Long.MAX_VALUE);
            }
        }

        myHtml.append("</span>\n");
        myHtml.append("</pre>\n");
    }


    public void renderMarkedUpSource(File where)
        throws IOException
    {
        myHtml = new HtmlWriter(where);

        myHtml.renderHeadWithInlineCss("Fusion Code Coverage", CSS);

        myHtml.append("<p>Report generated at ");
        myHtml.append(Timestamp.now().toString());
        myHtml.append("</p>\n");

        SourceName mainSourceName = null;
        if (mySourceFile != null)
        {
            mainSourceName = SourceName.forFile(mySourceFile);
            renderSource(mainSourceName);
        }

        for (SourceName name : myCollector.sortedNames())
        {
            if (! name.equals(mainSourceName) && name.getFile() != null)
            {
                myHtml.append("<hr/>");

                renderSource(name);
            }
        }

        myHtml.append("<hr/>");

        long total = myCoveredExpressions + myUncoveredExpressions;
        BigDecimal percentCovered =
            new BigDecimal(myCoveredExpressions * 100).divide(new BigDecimal(total),
                                                             2,
                                                             RoundingMode.HALF_EVEN);
        myHtml.append(myCoveredExpressions + " expressions observed<br/>");
        myHtml.append(percentCovered + "% expression coverage");

        myHtml.close();
    }
}
