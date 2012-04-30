<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:template match="programlisting[@role='httprequest']">
  <div class="formalcodebox">
    <div class="formalcodebox-title">HTTP Request</div>
    <pre class="programlisting">
      <xsl:apply-templates/>
    </pre>
  </div>
</xsl:template>

<xsl:template match="programlisting[@role='httpresponse']">
  <div class="formalcodebox">
    <div class="formalcodebox-title">HTTP Response</div>
    <pre class="programlisting">
      <xsl:apply-templates/>
    </pre>
  </div>
</xsl:template>

<xsl:template match="programlisting[@role='jsonsample']">
  <div class="formalcodebox">
    <div class="formalcodebox-title">JSON</div>
    <pre class="programlisting">
      <xsl:apply-templates/>
    </pre>
  </div>
</xsl:template>

<xsl:template match="programlisting[@role='memcrequest']">
  <div class="formalcodebox">
    <div class="formalcodebox-title">memcached Request</div>
    <pre class="programlisting">
      <xsl:apply-templates/>
    </pre>
  </div>
</xsl:template>

<xsl:template match="programlisting[@role='memcresponse']">
  <div class="formalcodebox">
    <div class="formalcodebox-title">memcached Response</div>
    <pre class="programlisting">
      <xsl:apply-templates/>
    </pre>
  </div>
</xsl:template>

</xsl:stylesheet>
