var  init  =  require ( 'init-package-json' ) 
var  yol  =  gerekli ( 'yol' )

// bir promzard modülüne giden yol. Bu dosyanın 
// bulunmaması durumunda size bir tane verilecektir. 
var  initFile  =  yol . çözümleme ( process . env . HOME ,  ".npm-init" )

// bir şeyler yaptığımız yer. 
var  dir  =  işlem . cwd ( )

// PromZard modülünün içeriğine eklenen ekstra şeyler. 
// npm'de, bu çözümlenmiş yapılandırma nesnesidir. 'Config' olarak gösterildi 
// İsteğe bağlı. 
var  configData  =  {  bazı : 'fazladan şeyler'  }

// package.json dosyasındaki mevcut her şey, 
// PromZard modülünde "package" nesnesi olarak gösterilir. Ayrıca 
paket.json dosyasının : 
// * `dosyaadı` yolu için 
üç // değişken olacaktır // *` basename` paketin ipucu dir 
// * `dirname` paket dizinin ebeveyni

init ( dir ,  initFile ,  configData ,  function  ( er ,  data )  { 
  // veriler zaten {dir} /package.json'a yazılmıştır 
  // artık onunla bir şeyler yapabilirsiniz 
} )
