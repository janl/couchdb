<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:import href="docbook-xsl/epub/docbook.xsl" />
<xsl:import href="docbook-xsl/html/highlight.xsl"/>
<xsl:import href="extensions.xsl"/>

<xsl:output method="html" encoding="utf-8" indent="yes"/>

<xsl:param name="section.autolabel" select="1"/>
<xsl:param name="section.label.includes.component.label" select="1"/>

<xsl:template match="remark" />

<xsl:template match="programlisting[@role='httprequest']">
  <div class="httprequest">
    <div class="httprequest-title">HTTP Request</div>
    <pre class="programlisting">
    <xsl:copy>
      <xsl:apply-templates select="* | text() | @* | processing-instruction()"/>
    </xsl:copy>
    </pre>
  </div>
</xsl:template>

<xsl:template match="programlisting[@role='httpresponse']">
  <div class="httpresponse">
    <div class="httpresponse-title">HTTP Response</div>
    <pre class="programlisting">
    <xsl:copy>
      <xsl:apply-templates select="* | text() | @* | processing-instruction()"/>
    </xsl:copy>
    </pre>
  </div>
</xsl:template>

<xsl:template match="programlisting[@role='jsonsample']">
  <div class="jsonsample">
    <div class="jsonsample-title">JSON</div>
    <pre class="programlisting">
    <xsl:copy>
      <xsl:apply-templates select="* | text() | @* | processing-instruction()"/>
    </xsl:copy>
    </pre>
  </div>
</xsl:template>

</xsl:stylesheet>
