library(jsonlite)
library(httr)
library(plyr)
library(jiebaR)
library(tm)

homeurl <- "http://www.bagualu.net/wordpress/wp-json/wp/v2/posts"
markdownRoot <- "/home/xuyang/blog/"
rwpEnv<-new.env()


mixseg = worker()
key_worker = worker("keywords",topn=3)

#' list the blog ids for the given page
#' 
#' description
#' 
#' @param page value
#' @return returndes
#' @export 
#' @examples 
#' x=c(1,2,3) 
listblogIds<-function(page)
{
    print(paste0("fetch page ",page," ..."))
    pageurl=paste0(homeurl,"?page=",page)
    print(pageurl)
    yy<-httr::GET(pageurl)
    print(yy)
    con=jsonlite::fromJSON(httr::content(yy,as="text",encoding="utf-8"))
    #con=fromJSON(content(yy))
    con[,c("id")]
}

#' get the blog title and content for given id
#' 
#' the id should be one of the fetched id
#' 
#' @param id value
#' @return list(title=,content=)
#' @export 
#' @examples 
#' x=c(1,2,3) 
getBlog<-function(id)
{
    aurl=paste0(homeurl,"/",id)
    yy<-httr::GET(aurl)
    jj=jsonlite::fromJSON(httr::content(yy,"text",encoding="utf-8"))
    list(title=jj$title$rendered,content=jj$content$rendered)
}

#' return the title and content in vec
#' 
#' description
#' 
#' @param id value
#' @return returndes
#' @export 
#' @examples 
#' x=c(1,2,3) 
getBlogV<-function(id)
{
	bb=getBlog(id)
	c(id,unlist(bb))
}

#' update blog with new content, do not use it now
#' 
#' description
#' 
#' @param id value
#' @param title value
#' @param content value
#' @return returndes
#' @export 
#' @examples 
#' x=c(1,2,3) 
updateBlog<-function(id)
{
	murl=paste0(homeurl,"/",id)
	#use curl instead
        system2("curl",c("--user", "admin:Helloworld1","-X","POST",murl,"-d",paste0("content=[bgurl]markdown/p",id,".html[/bgurl]")))
}

#' title 
#' 
#' description
#' 
#' @param id value
#' @return returndes
#' @export 
#' @examples 
#' x=c(1,2,3) 
needConv<-function(id)
{
    print(paste0("chekcing artical ",id," ..."))
    pageurl=paste0(homeurl,"/",id)
    yy<-GET(pageurl)
    jj=fromJSON(content(yy,"text",encoding="utf-8"))
    res=c(jj$title$rendered,jj$content$rendered)
    done=grep("bgurl",res)
    ret = 1
    if(length(done) >= 1) {
        ret = 0 
    }
    c(id,ret)
}

#' fetch the blog IDs , save it in the ./allids.Rds
#' 
#' description
#' 
#' @return returndes
#' @export 
#' @examples 
#' x=c(1,2,3) 
fetchBlogIds<-function()
{
	pages=1:70
	allblogs <- unlist(plyr::mlply(pages,listblogIds))
        saveRDS(allblogs,"./allids.Rds")
}

#' load the allids.Rds and return it
#' 
#' description
#' 
#' @return returndes
#' @export 
#' @examples 
#' x=c(1,2,3) 
loadBlogIds<-function()
{
	readRDS("./allids.Rds")
}

#' title 
#' 
#' description
#' 
#' @param id value
#' @return returndes
#' @export 
#' @examples 
#' x=c(1,2,3) 
extractKeyWords<-function(id)
{
	print(paste0("processing ", id, " ..."))
	con=getBlog(id)
	con$content=substr(con$content,20,nchar(con$content)-13)
	#replace the .html to .md
	con$content = gsub(".html",".md",con$content)
	mdfile=paste0("./",con$content)
	kk = keys[mdfile]
        if(length(grep("md",kk)) >= 1){
	    con$content = gsub(".md",".rmd",con$content)
	    mdfile=paste0("./",con$content)
	    kk = keys[mdfile]
	}
	#remove any keywords that length > 15
	kk = kk[nchar(kk) < 15]
	kks=paste(kk,collapse="/")
	c(id,unlist(con),kks)
}

#' title 
#' 
#' description
#' 
#' @param ids value
#' @return returndes
#' @export 
#' @examples 
#' x=c(1,2,3) 
calcKeywords<-function(ids)
{
	rr=ldply(ids,extractKeyWords)
	rr$link=sprintf("[%s](%s)",rr$title,paste0("http://www.bagualu.net/wordpress/archives/",rr$V1))
	kk=rr[,c("link","V2")]
        colnames(kk)=c("文章","关键词")
	saveRDS(kk,"./p6127.Rds")
}

#' title 
#' 
#' list the ids that need to be converted
#' 
#' @return returndes
#' @export 
#' @examples 
#' x=c(1,2,3) 
IdsNeedConv<-function()
{
	allids = readRDS("./allids.Rds")
	nids=ldply(allids,needConv)
	saveRDS(nids,"needIds.Rds")
}

#' title 
#' 
#' description
#' 
#' @param id value
#' @return returndes
#' @export 
#' @examples 
#' x=c(1,2,3) 
genHtml<-function(id){

    aurl=paste0(homeurl,"/",id)
    yy<-GET(aurl)
    jj=fromJSON(content(yy,"text",encoding="utf-8"))
    #print(jj)
    res=c(jj$title$rendered,jj$content$rendered)
    #generate the html
    filename=paste0("p",id,".html")
    cat(file=filename,res[2])
    return(res) 

}

#convert one blog from html to markdown
#' title 
#' 
#' description
#' 
#' @param id value
#' @return returndes
#' @export 
#' @examples 
#' x=c(1,2,3) 
convOneBlog<-function(id)
{
    aurl=paste0(homeurl,"/",id)
    yy<-GET(aurl)
    jj=fromJSON(content(yy,"text",encoding="utf-8"))
    res=c(jj$title$rendered,jj$content$rendered)
    #generate the html
    filename=paste0("./p",id,".html")
    htmlfile=filename
    cat(file=filename,res[2])

    #to see if there is "[sourcecode" or "[code" exist 

    #convert it to markdown

    #process the html file
    fcontent=readLines(filename)

    #if find bgurl, just return
    done=grep("bgurl",fcontent)
    if(length(done) >= 1) {
        print("it is converted already")
    }else{

    ##check the [sourcecode ...] and [code ...]
    #codes=c("c","php","python","bash","html","xml","java","javascript","perl")
    #matches=sprintf("<pre class='sourceCode %s'><code class='sourceCode %s'>",codes,codes)
    ##names(matches) = sprintf("\\[code=&#8217;%s&#8217;\\]",codes)
    #names(matches) = sprintf("\\[sourcecode language=&#8217;%s&#8217;\\]",codes)
    #
    #print(matches)
    #fcontent=stringr::str_replace_all(fcontent,matches)

    ##replace the [/code]
    #matches=c("</code></pre>")
    ##names(matches)=c("\\[\\/code\\]")
    #names(matches)=c("\\[\\/sourcecode\\]")
    #print(matches)
    #fcontent=stringr::str_replace_all(fcontent,matches)

    #newhtmlfile=paste0(filename,"1")
    #cat(fcontent,file=newhtmlfile,sep="\n")
    #print("conversion done")

    #mdfile=paste0("./p",id,".md")
    #system2("pandoc",c("-f","html","-t","markdown","-o",mdfile,newhtmlfile))

    mdfile=paste0("./p",id,".md")
    system2("pandoc",c("-f","html","-t","markdown","-o",mdfile,htmlfile))

    system2("cp",c(mdfile,"/home/xuyang/blog/"))

    #update the blog content
    #set the blog content
    #updateBlog(id,title,content)
    updateBlog(id)
    

    #update the blog and copy the generated html
    #gen the html again
    #system2("pandoc",c("-t","html","-f","markdown","-s","-o",filename,newmdfile))
    } 
}

#' title 
#' 
#' description
#' 
#' @param V value
#' @return returndes
#' @export 
#' @examples 
#' x=c(1,2,3) 
convBlog<-function(V)
{
	if(V[2]) {
	    print(paste0("converting ",V[1]))
	    convOneBlog(V[1])
	}
	
}
 
#' title 
#' 
#' description
#' 
#' @return returndes
#' @export 
#' @examples 
#' x=c(1,2,3) 
convBlogs<-function(){
	nids=readRDS('./needIds.Rds')
	#dlply(nids[,c("V1","V2")],.(V1,V2),convBlog)
	apply(nids[,c("V1","V2")],1,convBlog)
}


#' build a book with the given ids
#' 
#' each id is a chapter
#' 
#' @param ids value
#' @param name value
#' @param title value
#' @param copyright the copyright md file for the book
#'        the file should be at the markdownRoot folder
#' @param preface : the preface md file for the book
#' @return returndes
#' @export 
#' @examples 
#' x=c(1,2,3) 
buildbook<-function(ids,name="~/wpbook",title="",copyright="copyright.md",preface="preface.md")
{
	#get the titles and the markdown file
	con=plyr::ldply(ids,getBlogV)
	#get the markdown file from the content
	mds=stringr::str_extract(con$content,"p\\d+")
	finalfile=unlist(plyr::mlply(mds,matchfile))
	#create the folder for the new book
	print(finalfile)
	if(file.exists(name)) {
		system2("rm",c(name,"-rf"))
	}

	system2("mkdir",c(name,"-p"))
	system2("mkdir",c(paste0(name,"/rfigures"),"-p"))
	file.copy(finalfile,name,overwrite=TRUE)

	file.copy(paste0(markdownRoot,copyright),name,overwrite=TRUE)
	file.copy(paste0(markdownRoot,preface),name,overwrite=TRUE)
	#adding the title
	#get the title firstly
	oldwd=getwd()
	setwd(name)
	mdfiles_in_newplace=dir(name,'*md')
	plyr::m_ply(mdfiles_in_newplace,addtitle)
	
	#compile the file to tex
	for (x in  mdfiles_in_newplace) {
		rmarkdown::render(x,bookdown::tex_chapter())
	}
	
	#fetch the template
	template=system.file("","template.tex",package='rwp')
	print(template)
	system2("cp",c(template,name,"-fv"))
	
	#generate the template for the book
	#insert the mds into the tex template
	chapters=sprintf("\\include{%s}",mds)
	
	rawtemplate=readLines("./template.tex")

	if (nchar(title) > 1 ){
		rawtemplate=sub('xxtitle',title,rawtemplate)
	}

	ll=grep('xxchapter',rawtemplate)
	if(length(ll) == 1) {
		rawtemplate[ll] = paste(chapters,collapse="\n")
	}
	writeLines(rawtemplate,"book.tex")
	
	#generate the pdf
	#xelatex -interaction=batchmode mybook
	system2("xelatex",c("-interaction=batchmode","book"))
	#the 2nd pass for the index
	system2("xelatex",c("-interaction=batchmode","book"))
	setwd(oldwd)
	print(paste("book.pdf is generated in", name,"folder"))
}

#' find the md or rmd file for given p123
#' 
#' description
#' 
#' @param x value
#' @return returndes
#' @export 
#' @examples 
#' x=c(1,2,3) 
matchfile<-function(x)
{
	#give the p123.* 
	ffs=dir(markdownRoot,pattern=paste0(x,".(md|rmd|tbl|Rds)$"))
	sprintf("%s%s",markdownRoot,ffs)
	#mdfile=sprintf("%s%s.md",markdownRoot,x)
	#rmdfile=sprintf("%s%s.rmd",markdownRoot,x)
	#if( file.exists(mdfile) ){
	#	mdfile
	#}else if ( file.exists(rmdfile) ) {
	#	rmdfile
	#}else{
	#	NA
	#}
}


#' fetch the blog title and insert it into the md file
#' 
#' description
#' 
#' @param fname input file name, md or rmd file
#' @return returndes
#' @export 
#' @examples 
#' x=c(1,2,3) 
addtitle<-function(fname){
	tt=stringr::str_extract(fname,'\\d+')
	print(paste("fetch title",tt))
	bb=getBlog(tt)
	system2("sed",c("-ie",paste0("'1 i\\# ",bb$title,"'"),fname))
}

#' generate table for given dataframe
#' 
#' description
#' 
#' @param x data frame 
#' @return returndes
#' @export 
#' @examples 
#' x=c(1,2,3) 
blogtable<-function(x)
{
	if(is_latex()) {
	        knitr::kable(x,format="latex",align="c")
	}else{
        	knitr::kable(x,format="html",table.attr = "class=\"table table-bordered\"", align="c")
	}
}

#' target doc type for rmarkdown
#' 
#' description
#' 
#' @return returndes
#' @export 
#' @examples 
#' x=c(1,2,3) 
doc_type <- function() {
	knitr::opts_knit$get('rmarkdown.pandoc.to')
}
#' is target type latex ?
#' 
#' description
#' 
#' @return returndes
#' @export 
#' @examples 
#' x=c(1,2,3) 
is_latex <- function() {
  identical(doc_type(), "latex")
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' embed png in the markdown
#' 
#' description
#' 
#' @param path value
#' @param dpi value
#' @return returndes
#' @export 
#' @examples 
#' x=c(1,2,3) 
embed_png<- function(path, dpi = 200) {
  if( is_latex() ){
	#copy the image to local disk
	#get the imgname
	tts=strsplit(path,"/")[[1]]
	dfile=tts[length(tts)]
	if(! file.exists("./images")){
		system2("mkdir",c("./images"))
	}
	#print(paste("downloading image","./images/",dfile))
	curl::curl_download(path,paste0("./images/",dfile))
	path = paste0("./images/",dfile)

        meta <- png_meta(path)
        dpi <- dpi %||% meta$dpi[1] %||% stop("Unknown dpi", call. = FALSE)

    width <- round(meta$dim[1] / dpi, 2)

    knitr::asis_output(paste0(
      "\\includegraphics[",
      "width=", width, "in",
      "]{", path, "}"
    ))
  } else {

        #meta <- png_meta(path)
        #dpi <- dpi %||% meta$dpi[1] %||% stop("Unknown dpi", call. = FALSE)

    knitr::asis_output(paste0(
      "<img src='", path, "'/>"
      #" width='", round(meta$dim[1] / (dpi / 96)), "'",
      #" height='", round(meta$dim[2] / (dpi / 96)), "'",
      #" />"
    ))
  }
}


#' return the png meta data given the path
#' 
#' description
#' 
#' @param path value
#' @return returndes
#' @export 
#' @examples 
#' x=c(1,2,3) 
png_meta <- function(path) {
  attr(png::readPNG(path, native = TRUE, info = TRUE), "info")
}

