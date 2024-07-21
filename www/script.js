
function fox(val) {
	
  var copyText = val;
	
   /* Copy the text inside the text field */
  navigator.clipboard.writeText(copyText);
  
  showSnackBar()
} 


function showSnackBar() {
  var sb = document.getElementById("snackbar");

  //this is where the class name will be added & removed to activate the css
  sb.className = "show";

  setTimeout(()=>{ sb.className = sb.className.replace("show", ""); }, 1900);
}