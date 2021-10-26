---
title: "TOV-E Bird monitoring sampling data"
output:
  html_document:
    df_print: paged
  pdf_document: default
---

## A Mini-Introduction to R Markdown

Markdown documents are text files that contain a mixture of standard prose and programming code that can be easily be rendered as HTML or PDF outputs.  During this rendering process, any code blocks are run and the results embedded in the output document.  This can be really useful for making figures and tables to help describe a data set and to document analyses.  [R markdown](https://rmarkdown.rstudio.com/) is a flavour of markdown that interfaces with the [R statistical platform](https://www.r-project.org/).

Code can included in R markdown documents in 'chunks' such as the one shown below:



However, you will only see this code chunk in the original markdown text.  At the top of the code chunk you will see the line `input=FALSE`.  This means that the code is run but that neither the code itself or the results of the code is printed when rendering the document to HTML or any other output format.  This is useful when you want to set up things in the background that you don't want rendered into the document.  In this example the code chunk above calls the libraries that our markdown document will use so we don't need to have that code rendered to the HTML output.  We can also make the code chunks print to the rendered document by removing the `include=FALSE` term.  Sometimes, you might only want the result to be created in the rendered document.  For example, when producing figures, we often do not want the code that produces the figure in the rendered document, just the figure itself.  By setting `echo=FALSE` in the code chunk options then the code itself will not be rendered in the output document.  For example the code chunk below will appear as code in the markdown document but will be replaced by a figure in the rendered document.

<img src="C:/Users/joseph.chipperfield/AppData/Local/Temp/RtmpklQzlO/preview-1144769464a4.dir/LNWorkshopExample_TOV-E_Metadata_files/figure-html/figExample-1.png" width="672" />

The easiest way to render this document is to open it in [RStudio](https://www.rstudio.com/) and then click on the 'Knit' button that is in the top left-hand corner. A rendered version of the document is then opened in a browser.

We can also call R functions 'inline'.  For example in the markdown version of the document the term: 4 appears as 'r 2+2' but, in the rendered version, R is called and the calculation is performed.  The statement is then replaced by the output (in this case '4').  A full introduction to R markdown is beyond the scope of this document but there are number of [great resources](https://bookdown.org/yihui/rmarkdown/) to learn more.

When using markdown to describe the metadata of the project we can use a number of functions described in the Living Norway package to help flag sections of text that we want to export as information to appear in an EML file.  The code chunk below will allow you to see flagged sections of text being rendered in a red colour in HTML output.  For normal descriptions of metadata you would not want this so you should delete this code chunk for your own metadata descriptions.


```css
span.LNmetadata {
  color: red;
}
```


<style type="text/css">
span.LNmetadata {
  color: red;
}
</style>

## The Dataset

Metadata must have a dataset tag.  We give this tag an ID and it serves as a parent ID for a lot of other tags that describe the dataset.  Often we don't want to print any text associated with this tag to the output so  we can therefore set the `isHidden` argument to `TRUE` so that the tag is invisible in the rendered output.  The main purpose of calling this function is to set an ID for the dataset tag that can be used to relate other things to it (here we have used the ID 'TOVEDataset').  In the dataset function we can also give information on the title to use in the dataset through the `title.tagText` argument.

<span id="LNdataset_TOVEDataset" class="LNmetadata" style="display:none"/><span id="LNtitle_72cba2fb-9df7-4c99-a574-7ae27d4cb5cc_TOVEDataset" class="LNmetadata" style="display:none">TOV-E Bird monitoring sampling data</span>

We can also associate some keywords with the dataset.  To do this we can set up a 'keywordSet' tag using the relevant tagging function <span id="LNkeywordSet_TOVEKeywordSet_TOVEDataset" class="LNmetadata" style="display:none"/> and then specifying keywords such as <span id="LNkeyword_4e9886a1-e7e8-4406-8b97-5923ed3c39c0_TOVEKeywordSet" class="LNmetadata">breeding birds</span> and <span id="LNkeyword_5e9bf0a1-8fad-4391-a7b1-f6fdf56c6204_TOVEKeywordSet" class="LNmetadata">sampling event</span>.

We must also specify some contact information for the individual or organisation responsible for coordinating with users of the dataset.  Here the responsible user is <span id="LNcontact_TOVEContact_TOVEDataset" class="LNmetadata"/> <span id="LNindividualName_456b4f78-f7e4-43b9-8ebc-26553ff60359_TOVEContact" class="LNmetadata"/><span id="LNgivenName_64d328cb-6918-451e-8333-1e9b567294b9_456b4f78-f7e4-43b9-8ebc-26553ff60359" class="LNmetadata">John Atle</span> <span id="LNsurName_0cdd6454-216c-4338-afbe-e334721a669a_456b4f78-f7e4-43b9-8ebc-26553ff60359" class="LNmetadata">Kålås</span>.

## Abstract

We will need to produce an abstract for the data.  You can flag the abstract for export to EML using the following inline code:

<span id="LNabstract_TOVEAbstract_TOVEDataset" class="LNmetadata">Data from the project: "Extensive monitoring of breeding birds (TOV-E)" from 2006 up until today.  The project is carried out in cooperation between NOF BirdLife Norway, Norwegian Institute for Nature Research (NINA) and Norwegian Environment Agency, and is the most important project for monitoring population trends for Norwegian bird species on land.</span>

The Living Norway package will also allow for alternative translations of EML elements.  In the inline code above we set the tagID argument.  We can provide an alternative translation for the element with that tagID using the following code:

<span id="LNvalue_d9e772f5-0882-4872-a8e8-219322c3946a_TOVEAbstract" class="LNmetadata" style="display:none" xml:lang="nb">Data fra prosjektet "Ekstensiv overvåking av hekkefugl (TOV-E)" fra 2006 og frem til i dag.  Prosjektet utføres i samarbeid mellom Norsk Ornitologisk Forening, Norsk Institutt for Naturforskning og Miljødirektoratet og er det viktigste prosjektet for å overvåke populasjonstrender for norske fuglearter på land.</span>

By default, alternative translations are hidden in rendered HTML output.  The information is still there but it is not displayed when being opened by a browser.  This is useful when you want information to be exported to the EML file but do not to display them in the rendered HTML.  If you would rather the alternative translation be displayed, then you can add the argument `isHidden=FALSE` to the `LNaddTranslation` function.

## Dataset creators

The dataset was created by the following people:

  + <span id="LNcreator_TOVECreator1_TOVEDataset" class="LNmetadata"/><span id="LNindividualName_d378d37c-e0fe-44be-b9cd-9e8d85f4a2f1_TOVECreator1" class="LNmetadata"/><span id="LNgivenName_66739475-2f1a-4f63-8759-ee757b88b422_d378d37c-e0fe-44be-b9cd-9e8d85f4a2f1" class="LNmetadata">John Atle</span> <span id="LNsurName_033a81b6-c947-46bf-b929-3dee4cd85605_d378d37c-e0fe-44be-b9cd-9e8d85f4a2f1" class="LNmetadata">Kålås</span> who is a <span id="LNpositionName_fb3ba8f2-20e0-431e-a7b4-ee712d8524da_TOVECreator1" class="LNmetadata">senior researcher</span> at the <span id="LNorganizationName_97982afb-9c20-4f57-823e-3fd25dd1800e_TOVECreator1" class="LNmetadata">Norwegian Institute for Nature Research</span> (<span id="LNelectronicMailAddress_e5452d43-b819-4286-b8e7-b4d18c5ca20c_TOVECreator1" class="LNmetadata">john.kalas@nina.no</span>).
  + <span id="LNcreator_TOVECreator2_TOVEDataset" class="LNmetadata"/><span id="LNindividualName_93b0a3e8-fd33-4665-8a60-b8b55035d850_TOVECreator2" class="LNmetadata"/><span id="LNgivenName_95cc00d6-1a92-40e0-ab66-964f4b802038_93b0a3e8-fd33-4665-8a60-b8b55035d850" class="LNmetadata">Ingar Jostein</span> <span id="LNsurName_dbe4ec43-2471-4386-b83b-d93f3ab169bb_93b0a3e8-fd33-4665-8a60-b8b55035d850" class="LNmetadata">Øien</span> who is a <span id="LNpositionName_c0193865-405e-40c0-b280-4f0efbc5ce49_TOVECreator2" class="LNmetadata">fagsjef</span> at the <span id="LNorganizationName_2a25a447-3365-4a56-b524-40fe98095b00_TOVECreator2" class="LNmetadata">Norsk Ornitologisk Forening</span> (<span id="LNelectronicMailAddress_cc80b2f4-af44-49be-8f28-7dc18a20f9c3_TOVECreator2" class="LNmetadata">ingar@birdlife.no</span>).
  + <span id="LNcreator_TOVECreator3_TOVEDataset" class="LNmetadata"/><span id="LNindividualName_7eb30bf5-d244-4e4d-ad09-893c9304d1a6_TOVECreator3" class="LNmetadata"/><span id="LNgivenName_8b77e8e8-dcd2-422f-92c6-0a8e123201b9_7eb30bf5-d244-4e4d-ad09-893c9304d1a6" class="LNmetadata">Bård</span> <span id="LNsurName_0a59e3c6-12e5-4d36-8d2a-03d99c80c07d_7eb30bf5-d244-4e4d-ad09-893c9304d1a6" class="LNmetadata">Stokke</span> who is a <span id="LNpositionName_8ac2f4b9-1835-47cf-8949-a3ca50fa2539_TOVECreator3" class="LNmetadata">senior researcher</span> at the <span id="LNorganizationName_4ca13792-f1e5-4279-adfb-b8f4d54f612d_TOVECreator3" class="LNmetadata">Norwegian Institute for Nature Research</span> (<span id="LNelectronicMailAddress_c620b6d7-e980-4c28-bf4c-b044c7655d2a_TOVECreator3" class="LNmetadata">bard.stokke@nina.no</span>).
  + <span id="LNcreator_TOVECreator4_TOVEDataset" class="LNmetadata"/><span id="LNindividualName_efd90d52-869f-4383-8a63-c6530a39de04_TOVECreator4" class="LNmetadata"/><span id="LNgivenName_d27cb798-48ec-49cd-a96c-0fb654ba4804_efd90d52-869f-4383-8a63-c6530a39de04" class="LNmetadata">Roald</span> <span id="LNsurName_340fd85a-2cb9-4e75-8b16-1783e0a8579b_efd90d52-869f-4383-8a63-c6530a39de04" class="LNmetadata">Vang</span> who is a <span id="LNpositionName_86880f1b-efe3-49e3-bbc5-4e9c55d87411_TOVECreator4" class="LNmetadata">data manager</span> at the <span id="LNorganizationName_ef2017cc-c648-4392-8e24-3e23ea92a982_TOVECreator4" class="LNmetadata">Norwegian Institute for Nature Research</span> (<span id="LNelectronicMailAddress_833dbaca-2be2-463c-b5da-e912dc9656b6_TOVECreator4" class="LNmetadata">roald.vang@nina.no</span>).
