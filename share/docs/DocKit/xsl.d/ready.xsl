<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:import href="docbook-xsl/lib/lib.xsl" />

  <xsl:output method="xml" version="1.0" encoding="utf-8" standalone="no"/>

<!-- Strip out remarks that are note, todo, or have no role -->

  <xsl:template
    match="remark[not(@role)]
          |remark[@role='note']
          |remark[@role='todo']
          |remark[@role='production-directive']
          ">
          </xsl:template>

<!-- Strip elements not marked for download publication -->

<xsl:template match="para[@revision='online']" />
<xsl:template match="section[@revision='online']" />
<xsl:template match="appendix[@revision='online']" />
<xsl:template match="wordasword[@revision='online']" />

<!-- Remove leading and trailing newlines -->

<xsl:template
match="programlisting/node()[1][self::text()][following-sibling::node()]">
 <xsl:call-template name="trim-left">
  <xsl:with-param name="contents" select="."/>
 </xsl:call-template>
</xsl:template>

<xsl:template match="programlisting/node()[position() =
last()][self::text()][preceding-sibling::node()]">
 <xsl:call-template name="trim-right">
  <xsl:with-param name="contents" select="."/>
 </xsl:call-template>
</xsl:template>

<xsl:template match="programlisting/node()[position() = 1 and position() =
last()][self::text()]"
            priority="1">
 <xsl:call-template name="trim.text">
  <xsl:with-param name="contents" select="."/>
 </xsl:call-template>
</xsl:template>

<!-- Preserve space -->

<xsl:preserve-space elements="*"/>

<!-- Pass through everything else as is -->

  <xsl:template match="/ | * | text() | @* | processing-instruction()">
    <xsl:copy>
      <xsl:apply-templates select="* | text() | @* | processing-instruction()"/>
    </xsl:copy>
  </xsl:template>

</xsl:stylesheet>

