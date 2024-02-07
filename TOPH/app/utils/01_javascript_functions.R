###################################################################################################
#
# Portal 173 - Javascript functions
#
###################################################################################################


jscode <- "
shinyjs.seque = function(par) {
    alert(par);	
};
shinyjs.downloadFile = function(params) {
	console.log('checkpoint1');
	const storage = firebase.storage();
	var defaultParams = {
        fileName : null,
        matterID : null
    };
	params = shinyjs.getParams(params, defaultParams);
	console.log('Documents/' + params.matterID + '/' + params.fileName);
	var storageRef = storage.ref('Documents/' + params.matterID + '/' + params.fileName);
	console.log('checkpoint3');
	storageRef
	.getDownloadURL()
	.then((url) => {
		console.log(url);
		const aElement = document.createElement('a');
		aElement.setAttribute('download', params.fileName);
		aElement.href = url;
		aElement.setAttribute('target', '_blank');
		aElement.click();
		console.log('Download complete');
	});
};
shinyjs.deleteFile = function(params) {
	const storage = firebase.storage();
	var defaultParams = {
        fileName : null,
        matterID : null
    };
	params = shinyjs.getParams(params, defaultParams);
	console.log('Documents/' + params.matterID + '/' + params.fileName);
	var storageRef = storage.ref('Documents/' + params.matterID + '/' + params.fileName);
	console.log('checkpoint3');
	storageRef
	.delete()
	.then(() => {
		console.log('Document deleted');
	});
};
shinyjs.expandBox = function(boxid) {
if (document.getElementById(boxid).parentElement.className.includes('collapsed-box')) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}};
shinyjs.collapseBox = function(boxid) {
if (!document.getElementById(boxid).parentElement.className.includes('collapsed-box')) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}};
"