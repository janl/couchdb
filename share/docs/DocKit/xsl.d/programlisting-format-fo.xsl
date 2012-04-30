<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:template match="programlisting[@role='httprequest']">
  <fo:inline font-weight="bold"
             keep-with-next.within-line="always"
             padding-end="1em">
    <xsl:text>HTTP Request</xsl:text>
    <xsl:apply-templates/>
  </fo:inline>
</xsl:template>
</xsl:template>

</xsl:stylesheet>
