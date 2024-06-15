jianpuly = __import__("jianpu-ly")

out = jianpuly.process_input("WithStaff 1 - 0 -")

assert "r2" in out
