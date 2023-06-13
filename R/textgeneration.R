textgeneration <- function(text_model, 
                                         prompt = NULL,
                                         pipeline_kwargs = list(),
                                         random_state = 42L,
                                         nr_docs = 4L,
                                         diversity = NULL) {
  
  reticulate::py_set_attr(model, "model", text_model)
  reticulate::py_run_string("model = r.model")
  reticulate::py_run_string("prompt = prompt")
  reticulate::py_run_string("pipeline_kwargs = pipeline_kwargs")
  reticulate::py_run_string("random_state = random_state")
  reticulate::py_run_string("nr_docs = nr_docs")
  reticulate::py_run_string("diversity = diversity")
  
  
  reticulate::py_run_string("\n
        set_seed(random_state)\n
        if isinstance(model, str):\n
            self.model = pipeline(\"text-generation\", model= model)\n
        elif isinstance(model, Pipeline):\n
            self.model = model\n
        else:\n
            raise ValueError(\"Make sure that the HF model that you pass is either a string referring to a HF model or a `transformers.pipeline` object.\")\n
        self.prompt = prompt if prompt is not None else DEFAULT_PROMPT\n
        self.default_prompt_ = DEFAULT_PROMPT\n
        self.pipeline_kwargs = pipeline_kwargs\n
        self.nr_docs = nr_docs\n
        self.diversity = diversity\n
  ")
}
